import canvas.data.AgentItem
import canvas.data.State

fun State.isBisimilarTo(other: State, states: List<State>) : Boolean {
    return recursiveBisimCheck(this, other, states)
}

private fun recursiveBisimCheck(state: State, other: State,
                                states: List<State>): Boolean {
    if (!atomsHolds(state, other))
        return false

    if (states.isEmpty() || state == other)
        return true


    val statesToCheck = states - listOf(state, other)

    val sReachable = buildReachableStateTuples(state, statesToCheck)
    val sPrimeReachable = buildReachableStateTuples(other, statesToCheck)

    //Forth clause
    val forth = checkKnowledgePreservation(sReachable, sPrimeReachable, statesToCheck)

    //Back clause
    if (forth)
        return checkKnowledgePreservation(sPrimeReachable, sReachable, statesToCheck)
    return false
}

/**
 * Checks if there is no such state that is either not reachable by the same agents or does
 * not satisfy the same propositions
 */
private fun checkKnowledgePreservation(reachableStates: Set<AgentListStateTuple>, otherReachableStates: Set<AgentListStateTuple>,
                                       markedStates: List<State>): Boolean {

    return reachableStates.all { reachableTuple ->
        otherReachableStates.any { otherTuple ->
            otherTuple.agents == reachableTuple.agents
                    && atomsHolds(otherTuple.state, reachableTuple.state)
                    && recursiveBisimCheck(otherTuple.state, reachableTuple.state, markedStates)
        }
    }
}

/**
 * Checks whether the Atoms clause in the definition of bisimilarity holds for the input states,
 * i.e they 'contain' identical sets of props
 */
private fun atomsHolds(s1: State, s2: State) = s1.props == s2.props

private fun buildReachableStateTuples(state: State, statesToCheck: List<State>): Set<AgentListStateTuple> {
    val inTuples = state.inEdges.map { AgentListStateTuple(it.inParent, it.agents) }
    val outTuples = state.outEdges.map { AgentListStateTuple(it.outParent, it.agents) }

    return (inTuples + outTuples)
            .filter { statesToCheck.contains(it.state) }
            .toSet()
}

private data class AgentListStateTuple(val state: State, val agents: List<AgentItem>)