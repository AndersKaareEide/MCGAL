package formulaParser

import canvas.data.AgentItem
import canvas.data.Model
import canvas.data.State
import formulaParser.formulaDebugger.Debugger
import sidepanels.debugpanel.DebugLabelItem
import sidepanels.debugpanel.FormulaLabelItem
import sidepanels.propertypanel.PropositionItem

/**
 * Based on an agent and a state, returns all states the given agent considers
 * indistinguishable from the given state
 */
//TODO Figure out if model is actually necessary
fun getIndishStates(agent: AgentItem, state: State): List<State> {
    val outEdges = state.outEdges.filter { it.agents.contains(agent) }
    val inEdges = state.inEdges.filter { it.agents.contains(agent) }

    val outStates = outEdges.map { it.parent1 }
    val inStates = inEdges.map { it.parent2 }

    return listOf(state) + inStates + outStates
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
fun buildSubformulaList(state: State, formula: Formula): List<Pair<State, Formula>> {
    return when (formula){
        is Proposition -> listOf(Pair(state, formula))
        is Negation -> listOf(Pair(state, formula)) + buildSubformulaList(state, formula.inner)
        is BinaryOperator -> listOf(Pair(state, formula)) +
                buildSubformulaList(state, formula.left) +
                buildSubformulaList(state, formula.right)
        is Knows -> {
            val indishStates = getIndishStates(formula.agent, state)
            val initial: List<Pair<State, Formula>> = listOf(Pair(state, formula))

            indishStates.map { buildSubformulaList(it, formula.inner) }
                    .fold(initial) { list, elements -> list.plus(elements) }
        }
        else -> TODO("Not implemented yet")
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

fun makeRange(needsParens: Boolean, start: Int, end: Int): Pair<Int, Int> {
    return if (needsParens) {
        Pair(start -1, end + 1)
    } else {
        Pair(start, end)
    }
}

fun insertParentheses(list: MutableList<FormulaLabelItem>, formula: Formula){
    val size = list.size + 1
    list.add(0, FormulaLabelItem(formula, "(", Pair(0, size)))
    list.add(FormulaLabelItem(formula, ")", Pair(-size, 0)))
}

fun insertParentheses(map: MutableMap<State, MutableList<DebugLabelItem>>, formula: Formula, state: State){
    val size = map.size + 1
    map[state]!!.add(0, DebugLabelItem(formula, "(", Pair(0, size), state))
    map[state]!!.add(DebugLabelItem(formula, ")", Pair(-size, 0), state))
}

/**
 * Function that combines two stateToLabel maps by combining the label lists where keys overlap
 * Candidate for ugliest function ever written in Kotlin
 */
fun combineMapLists(leftMap: MutableMap<State, MutableList<DebugLabelItem>>,
                    rightMap: MutableMap<State, MutableList<DebugLabelItem>>):
        MutableMap<State, MutableList<DebugLabelItem>> {

    val overlappingKeys = leftMap.keys.filter { rightMap.containsKey(it) }
    val result = (leftMap + rightMap).toMutableMap()

    overlappingKeys.forEach {
        result.put(it, ((leftMap[it]!! + rightMap[it]!!).toMutableList()))
    }

    return result
}
