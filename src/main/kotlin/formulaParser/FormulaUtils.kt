package formulaParser

import canvas.data.AgentItem
import canvas.data.Edge
import canvas.data.Model
import canvas.data.State
import javafx.collections.ObservableList
import sidepanels.debugpanel.DebugLabelItem
import sidepanels.debugpanel.FormulaLabelItem

/**
 * Based on an agent and a state, returns all states the given agent considers
 * indistinguishable from the given state
 */
fun getIndishStates(agent: AgentItem, state: State, model: Model): List<State> {
    val inEdges = state.edges.filter { it.agents.contains(agent) }

    val inStates = inEdges.map { it.inParent }
    val outStates = inEdges.map { it.outParent }

    val result = setOf(state) + inStates + outStates
    return result.filter { model.states.contains(it) } //Handle updated models, states might be filtered out
}

fun Model.restrictedTo(stateList: List<State>) : Model {
    val filteredEdges = this.edges.filter {
        stateList.containsAll(listOf(it.inParent, it.outParent))
    }

    return Model(stateList, filteredEdges, this.agents, this.props)
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

fun getAbsoluteIntRange(debugLabelList: ObservableList<DebugLabelItem>, innerLabel: DebugLabelItem): IntRange {
    val opIndex = debugLabelList.indexOf(innerLabel)
    return IntRange(innerLabel.indexRange.first + opIndex, innerLabel.indexRange.last + opIndex)
}

fun getAnnounceableExtensions(model: Model, state: State, coalition: List<AgentItem>): List<List<State>> {
    val powerSet = generatePowerSetOfStates(model.states)

    return powerSet.filter { it.contains(state) }
            .filter { it.hasNoOverlappingEqClassesFor(coalition, model.edges)}
}

fun List<State>.hasNoOverlappingEqClassesFor(coalition: List<AgentItem>, edges: List<Edge>): Boolean =
        this.all { this.containsAll(it.eqClassIntersectionFor(coalition, edges)) }

fun State.eqClassIntersectionFor(agents: List<AgentItem>, actualEdges: List<Edge>): List<State> {
    if (agents.isEmpty())
        return listOf(this)

    return this.edges.filter { actualEdges.contains(it) && it.agents.containsAll(agents) }
            .map {
                if (it.inParent == this)
                    it.outParent
                else
                    it.inParent
            } + this
}

fun generatePowerSetOfStates(states: List<State>): List<List<State>>{
    val output = mutableListOf<List<State>>(listOf())

    var currentColumn: MutableList<List<State>>
    var nextColumn = states.map { listOf(it) }.toMutableList()

    for (length in 1 until states.size) {
        currentColumn = nextColumn
        nextColumn = mutableListOf()

        for(combination in currentColumn) {
            for (index in states.indexOf(combination.last()) + 1 .. states.lastIndex){
                nextColumn.add(combination + states[index])
            }
        }
        output.addAll(currentColumn)
    }
    output.addAll(nextColumn)

    return output
}



