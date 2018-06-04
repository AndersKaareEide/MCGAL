import canvas.data.AgentItem
import canvas.data.Edge
import canvas.data.Model
import canvas.data.State

private data class AgentListStateTuple(val state: State, val agents: List<AgentItem>)

fun State.isBisimilarTo(other: State, states: List<State>) : Boolean {
    return recursiveBisimCheck(this, other, states)
}

private fun recursiveBisimCheck(state: State, other: State,
                                states: List<State>): Boolean {
    if (!atomsHolds(state, other))
        return false

    if (states.isEmpty() || state == other)
        return true

    val nextStates = states - listOf(state, other)

    val sReachable = buildReachableStateTuples(state, states)
    val sPrimeReachable = buildReachableStateTuples(other, states)

    //Forth clause
    val forth = checkKnowledgePreservation(sReachable, sPrimeReachable, nextStates)

    //Back clause
    if (forth)
        return checkKnowledgePreservation(sPrimeReachable, sReachable, nextStates)
    return false
}

/**
 * Checks if there is no such state that is either not reachable by the same agents or does
 * not satisfy the same propositions
 */
private fun checkKnowledgePreservation(reachableStates: Set<AgentListStateTuple>,
                                       otherReachableStates: Set<AgentListStateTuple>,
                                       nextStates: List<State>): Boolean {

    return reachableStates.all { reachableTuple ->
        reachableTuple.agents.all { agent ->
            otherReachableStates.any { otherTuple ->
                otherTuple.agents.contains(agent)
                        && recursiveBisimCheck(otherTuple.state, reachableTuple.state, nextStates)
            }
        }
    }
}

/**
 * Checks whether the Atoms clause in the definition of bisimilarity holds for the input states,
 * i.e they 'contain' identical sets of props
 */
private fun atomsHolds(s1: State, s2: State) = s1.props == s2.props

private fun buildReachableStateTuples(state: State, statesToCheck: List<State>): Set<AgentListStateTuple> {
    val tuples = state.edges.map { if (it.inParent == state){
        AgentListStateTuple(it.outParent, it.agents)
    } else {
        AgentListStateTuple(it.inParent, it.agents)
    }
    }

    return tuples
            .filter { statesToCheck.contains(it.state) }
            .toSet()
}

/**
 * Creates a new model without any bisimilar states by merging all bisimilar states in the
 * input model, creating new remapped edges as necessary
 */
fun bisimContract(model: Model): Model {
    val originalToMergedStateMap = mergeBisimilarStates(model)

    val clonedStates = model.states
            .associate { Pair(it, State(it.name, props = it.props)) }
            .filter { !originalToMergedStateMap.contains(it.key) }

    val mergedEdges = mergeEdges(model.edges, clonedStates, originalToMergedStateMap)

    val states = originalToMergedStateMap.values.distinct() + clonedStates.values
    return (Model(states, mergedEdges.toMutableList(), model.agents, model.props))
}

private fun mergeBisimilarStates(model: Model): Map<State, State> {
    val bisimStatesMap = mapBisimilarStates(model)
    val mergedStates = bisimStatesMap.values.distinct()
            .map { mergeStates(it) }

    //We want a mapping of every original state pointing to the same merged state
    val mergeMap = mutableMapOf<State, State>()
    for (pair in mergedStates){
        for (originalState in pair.first){
            mergeMap[originalState] = pair.second
        }
    }

    return mergeMap
}

private fun mapBisimilarStates(model: Model): MutableMap<State, MutableSet<State>> {
    //TODO Optimize so shit isn't calculated twice s1 <-> s4, s4 <-> s1
    val bisimStatesMap = mutableMapOf<State, MutableSet<State>>()

    for (state in model.states) {
        for (otherState in model.states) {
            if (state != otherState && recursiveBisimCheck(state, otherState, model.states)) {
                if (bisimStatesMap.containsKey(state)) {
                    bisimStatesMap[state]!!.add(otherState)
                } else if (bisimStatesMap.values.any { it.contains(otherState) }) {
                    val otherSet = bisimStatesMap.values.first { it.contains(otherState) }
                    bisimStatesMap[state] = otherSet
                } else {
                    bisimStatesMap[state] = mutableSetOf(state, otherState)
                }
            }
        }
    }
    return bisimStatesMap
}

private fun mergeStates(bisimilarStates: Set<State>): Pair<Set<State>, State> {
    val stateName = bisimilarStates.map { it.name }
            .reduce { acc, next -> acc + next}

    val mergedState = State(stateName, props = bisimilarStates.first().props) //Since states are bisimilar, they have same props
    return Pair(bisimilarStates, mergedState)
}

private fun mergeEdges(oldEdges: List<Edge>, copiedStates: Map<State, State>, mergeMap: Map<State, State>): MutableSet<Edge> {
    val mergedEdges = mutableSetOf<Edge>()
    for (oldEdge in oldEdges) {
        val inParent = copiedStates[oldEdge.inParent] ?: mergeMap[oldEdge.inParent]!!
        val outParent = copiedStates[oldEdge.outParent] ?: mergeMap[oldEdge.outParent]!!

        if (inParent != outParent) {
            mergedEdges.add(Edge.makeEdgeBetween(inParent, outParent, oldEdge.agents))
        }
    }
    return mergedEdges
}
