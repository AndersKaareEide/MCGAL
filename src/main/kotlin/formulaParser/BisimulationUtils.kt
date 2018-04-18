import canvas.data.AgentItem
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
    val inTuples = state.edges.map { AgentListStateTuple(it.inParent, it.agents) }

    return inTuples
            .filter { statesToCheck.contains(it.state) }
            .toSet()
}

