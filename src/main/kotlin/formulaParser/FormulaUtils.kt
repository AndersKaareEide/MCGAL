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

fun insertParentheses(map: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>>, formula: Formula, state: State, index: Int){
    val size = map.size + 1
    map[state]!![index].add(0, DebugLabelItem(formula, "(", Pair(0, size), state))
    map[state]!![index].add(DebugLabelItem(formula, ")", Pair(-size, 0), state))
}

/**
 * Function that combines two stateToLabel maps by combining the label lists where keys overlap
 * Candidate for ugliest function ever written in Kotlin
 */
fun combineMapLists(leftMap: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>>,
                    rightMap: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>>):
        MutableMap<State, MutableList<ObservableList<DebugLabelItem>>> {

    val overlappingKeys = leftMap.keys.filter { rightMap.containsKey(it) }
    val result = (leftMap + rightMap).toMutableMap()

    for (key in overlappingKeys){
        val combinedEntries = mutableListOf<ObservableList<DebugLabelItem>>()
        var index = 0
        if (leftMap[key]!!.size >= rightMap[key]!!.size){
            while (index <= rightMap[key]!!.lastIndex){ //List level
                combinedEntries.add(FXCollections.observableArrayList(leftMap[key]!![index] + rightMap[key]!![index]))
                index++
            }
            while (index <= leftMap[key]!!.lastIndex){
                combinedEntries.add(leftMap[key]!![index])
                index++
            }
        } else {
            while (index <= leftMap[key]!!.lastIndex){
                combinedEntries.add(FXCollections.observableArrayList(leftMap[key]!![index] + rightMap[key]!![index]))
                index++
            }
            while (index <= rightMap[key]!!.lastIndex){
                combinedEntries.add(rightMap[key]!![index])
                index++
            }
        }
        result.put(key, combinedEntries)
    }
    return result
}

/**
 * Father, I have sinned
 */
fun initializePropositionList(currentIndex: Int): ArrayList<ObservableList<DebugLabelItem>> {
    val list = ArrayList<ObservableList<DebugLabelItem>>(currentIndex + 1)
    for (index in 0 .. currentIndex){
        list.add(FXCollections.observableArrayList())
    }
    return list
}

/**
 * Massively dirty function for checking if a state's list of labelLists already contains this
 */
fun equalityTest(list1: ObservableList<DebugLabelItem>, prop: Proposition): Boolean {
    if (list1.size != 1)
        return false

    val otherProp = list1[0].formula
    return otherProp is Proposition && otherProp.proposition.propString == prop.proposition.propString
}