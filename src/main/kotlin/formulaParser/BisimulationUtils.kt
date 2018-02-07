import canvas.data.AgentItem
import canvas.data.State

fun State.isBisimilarTo(other: State) : Boolean {
    if (!atomsHolds(this, other))
        return false

    val sReachable = buildReachableStateTuples(this)
    val sPrimeReachable = buildReachableStateTuples(other)

    //Forth clause
    val forth = checkKnowledgePreservation(sReachable, sPrimeReachable)

    //Back clause
    val back = checkKnowledgePreservation(sPrimeReachable, sReachable)

    return forth && back
}

/**
 * Checks if there is no such state that is either not reachable by the same agents or does
 * not satisfy the same propositions
 */
private fun checkKnowledgePreservation(reachableStates: Set<AgentListStateTuple>,
                                       otherReachableStates: Set<AgentListStateTuple>): Boolean {
    return reachableStates.all { reachableTuple ->
        //Forall reachable from state, there must be a bisimilar state reachable from other
        otherReachableStates.any {
            //Reachable by the same agents & contains the same atoms (propositions)
            it.agents == reachableTuple.agents && atomsHolds(it.state, reachableTuple.state)
        }
    }
}

/**
 * Checks whether the Atoms clause in the definition of bisimilarity holds for the input states,
 * i.e they 'contain' identical sets of props
 */
private fun atomsHolds(s1: State, s2: State) = s1.props == s2.props

private fun buildReachableStateTuples(state: State): Set<AgentListStateTuple> {
    val inTuples = state.inEdges.map { AgentListStateTuple(it.inParent, it.agents) }
    val outTuples = state.outEdges.map { AgentListStateTuple(it.outParent, it.agents) }

    return (inTuples + outTuples).toSet()
}

private data class AgentListStateTuple(val state: State, val agents: List<AgentItem>)
