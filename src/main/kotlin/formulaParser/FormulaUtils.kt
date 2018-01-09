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

/**
 * Based on an agent and a state, returns all states the given agent considers
 * indistinguishable from the given state
 */
//TODO Figure out if model is actually necessary, it is, fuck me
fun getIndishStates(agent: AgentItem, state: State, model: Model): List<State> {
    val outEdges = state.outEdges.filter { it.agents.contains(agent) }
    val inEdges = state.inEdges.filter { it.agents.contains(agent) }

    val outStates = outEdges.map { it.parent1 }
    val inStates = inEdges.map { it.parent2 }

    val result = listOf(state) + inStates + outStates
    return result.filter { model.states.contains(it) }
}

//TODO Decide how to handle model updates graphically
fun updateModel(announcement: Formula, model: Model, debugger: Debugger?): Model {
    val updStates = model.states.filter { announcement.check(it, model, debugger) }
    val updEdges = model.edges.filter { (updStates.contains(it.parent1) and updStates.contains(it.parent2)) }

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
//TODO Rewrite and connect states to formulas in order to handle announcements
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

            //TODO Optimize by building once and then mapping over? No, we need deep copies
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
fun poolGroupKnowledge(agents: List<AgentItem>, model: Model) : Model {
    val filteredEdges = model.edges.filter {
        it.agents.containsAll(agents)
    }

    return Model(model.states, filteredEdges, model.agents, model.props)
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

    val absRange = Debugger.getAbsoluteIntRange(debugLabelList, innerLabel)

    return FXCollections.observableArrayList(debugLabelList.slice(absRange))
}

fun getAnnouncementLabels(labelItem: DebugLabelItem, debugLabelList: ObservableList<DebugLabelItem>)
        : ObservableList<DebugLabelItem> {

    val announcement = (labelItem.formula as Announcement).announcement
    val announcementLabel = debugLabelList.find { it.formula == announcement }!!

    val absRange = Debugger.getAbsoluteIntRange(debugLabelList, announcementLabel)

    return FXCollections.observableArrayList(debugLabelList.slice(absRange))
}

