package formulaParser

import canvas.data.AgentItem
import canvas.data.Model
import canvas.data.State
import formulaParser.formulaDebugger.Debugger
import javafx.collections.FXCollections
import javafx.collections.ObservableList
import sidepanels.debugpanel.DebugLabelItem
import sidepanels.debugpanel.FormulaLabelItem
import sidepanels.propertypanel.PropositionItem
import tornadofx.*

/**
 * Based on an agent and a state, returns all states the given agent considers
 * indistinguishable from the given state
 */
fun getIndishStates(agent: AgentItem, state: State, model: Model): List<State> {
    val outEdges = state.outEdges.filter { it.agents.contains(agent) }
    val inEdges = state.inEdges.filter { it.agents.contains(agent) }

    val outStates = outEdges.map { it.inParent }
    val inStates = inEdges.map { it.outParent }

    val result = listOf(state) + inStates + outStates
    return result.filter { model.states.contains(it) } //Handle updated models, states might be filtered out
}

fun updateModel(announcement: Formula, model: Model, debugger: Debugger?): Model {
    val updStates = model.states.filter { announcement.check(it, model, debugger) }
    val updEdges = model.edges.filter { (updStates.contains(it.inParent) and updStates.contains(it.outParent)) }

    return Model(updStates, updEdges, model.agents, model.props)
}

/**
 * Extracts all the propositions the input formula is built on
 */
fun extractProps(formula: Formula): Set<PropositionItem> {
    return when (formula){
        is Proposition -> setOf(formula.proposition)
        is Negation -> extractProps(formula.inner)
        is BinaryOperator -> extractProps(formula.left) + extractProps(formula.right)
        is Knows -> extractProps(formula.inner)
        is Announcement -> extractProps(formula.inner)
        is GroupAnn -> extractProps(formula.inner)
        else -> throw RuntimeException("Missing branch in extractProps for ${formula.javaClass}")
    }
}

/**
 * Builds an immutable list of all the subformulas in the input formula, including the formula itself
 */
fun buildSubformulaList(state: State, formula: Formula, model: Model): List<Pair<State, Formula>> {
    return when (formula){
        is Proposition -> listOf(Pair(state, formula))
        is Negation -> listOf(Pair(state, formula)) + buildSubformulaList(state, formula.inner, model)
        is BinaryOperator -> listOf(Pair(state, formula)) +
                buildSubformulaList(state, formula.left, model) +
                buildSubformulaList(state, formula.right, model)
        is Knows -> {
            val indishStates = getIndishStates(formula.agent, state, model)
            val initial: List<Pair<State, Formula>> = listOf(Pair(state, formula))

            indishStates.map { buildSubformulaList(it, formula.inner, model) }
                    .fold(initial) { list, elements -> list.plus(elements) }
        }
        is Announcement -> {
            val initial: List<Pair<State,Formula>> = listOf(Pair(state, formula))
            val firstEntry = listOf(Pair(state, formula))

            val announcements =
                    model.states.map { buildSubformulaList(it, formula.announcement, model) }
                            .fold(initial){ list, elements -> list.plus(elements) }

            val innerEntries = buildSubformulaList(state, formula.inner, model)
            return firstEntry + announcements + innerEntries
        }
        else -> TODO("\nStepping through group announcement formulas is not implemented yet")
    }
}

/**
 * Checks if the input formula contains a K-operator or not
 */
fun containsKnowsOp(formula: Formula): Boolean {
    return when (formula){
        is Proposition -> false
        is Knows -> true
        is Negation -> containsKnowsOp(formula.inner)
        is BinaryOperator -> containsKnowsOp(formula.left) or containsKnowsOp(formula.right)
        is Announcement -> containsKnowsOp(formula.announcement) or containsKnowsOp(formula.inner)
        is GroupAnn -> containsKnowsOp(formula.inner)
        else -> throw RuntimeException("Missing branch in containsKnowsOp for ${formula.javaClass}")
    }
}

/**
 * Combines the knowledge of the given agents into an updated model by removing
 * uncertainties which are not shared by all the agents
 */
//TODO Un-retard-ify how
fun poolGroupKnowledge(agents: List<AgentItem>, model: Model) : Model {
    val filteredEdges = model.edges.filter {
        it.agents.containsAll(agents)
    }

    val updatedStates = mutableListOf<State>()
    for (state in model.states){
        val newState = State(state.name, state.xPos, state.yPos, state.props)
        newState.inEdges = state.inEdges.filter { filteredEdges.contains(it) }.observable()
        newState.outEdges = state.outEdges.filter { filteredEdges.contains(it) }.observable()

        updatedStates.add(newState)
    }

    return Model(updatedStates, filteredEdges, model.agents, model.props)
}

fun makeRange(needsParens: Boolean, start: Int, end: Int): IntRange {
    return if (needsParens) {
        IntRange(start -1, end + 1)
    } else {
        IntRange(start, end)
    }
}

fun insertParentheses(list: MutableList<FormulaLabelItem>, formula: Formula){
    val size = list.size + 1
    list.add(0, FormulaLabelItem(formula, "(", IntRange(0, size)))
    list.add(FormulaLabelItem(formula, ")", IntRange(-size, 0)))
}

fun getInnerLabels(knowsOp: DebugLabelItem, debugLabelList: ObservableList<DebugLabelItem>): ObservableList<DebugLabelItem> {
    val innerFormula = (knowsOp.formula as Knows).inner
    val innerLabel = debugLabelList.find { it.formula == innerFormula }!!

    val absRange = getAbsoluteIntRange(debugLabelList, innerLabel)

    return FXCollections.observableArrayList(debugLabelList.slice(absRange))
}

fun getAnnouncementLabels(labelItem: DebugLabelItem, debugLabelList: ObservableList<DebugLabelItem>)
        : ObservableList<DebugLabelItem> {

    val announcement = (labelItem.formula as Announcement).announcement
    val announcementLabel = debugLabelList.find { it.formula == announcement }!!

    val absRange = getAbsoluteIntRange(debugLabelList, announcementLabel)

    return FXCollections.observableArrayList(debugLabelList.slice(absRange))
}

fun getAbsoluteIntRange(debugLabelList: ObservableList<DebugLabelItem>, innerLabel: DebugLabelItem): IntRange {
    val opIndex = debugLabelList.indexOf(innerLabel)
    val absRange = IntRange(innerLabel.indexRange.first + opIndex, innerLabel.indexRange.last + opIndex)
    return absRange
}