import canvas.data.AgentItem
import canvas.data.State

fun State.isBisimilarTo(other: State) : Boolean {
    return recursiveBisimCheck(this, other, mutableSetOf())
}

private fun recursiveBisimCheck(state: State, other: State,
                                markedStates: MutableSet<StatePair>): Boolean {
    if (!atomsHolds(state, other))
        return false

    markedStates.add(StatePair(state, other))

    val sReachable = buildReachableStateTuples(state)
    val sPrimeReachable = buildReachableStateTuples(other)

    //Forth clause
    val forth = checkKnowledgePreservation(sReachable, sPrimeReachable, markedStates)

    //Back clause
    if (forth)
        return checkKnowledgePreservation(sPrimeReachable, sReachable, markedStates)
    return false
}

/**
 * Checks if there is no such state that is either not reachable by the same agents or does
 * not satisfy the same propositions
 */
private fun checkKnowledgePreservation(reachableStates: Set<AgentListStateTuple>, otherReachableStates: Set<AgentListStateTuple>,
                                       markedStates: MutableSet<StatePair>): Boolean {

    outer@ for (reachableTuple in reachableStates) {
        for (otherTuple in otherReachableStates) {
            if (markedStates.contains(StatePair(reachableTuple.state, otherTuple.state)))
                continue@outer
            else if (
                    otherTuple.agents == reachableTuple.agents
                    && atomsHolds(otherTuple.state, reachableTuple.state)
                    && recursiveBisimCheck(otherTuple.state, reachableTuple.state, markedStates)
            ) {
                continue@outer
            }
            return false
        }
        return true
    }
    return false
}

//    return reachableStates.all { reachableTuple ->
//        otherReachableStates.any { otherTuple ->
//            if (markedStates.contains(StatePair(reachableTuple.state, otherTuple.state)))
//                true
//            else {
//                otherTuple.agents == reachableTuple.agents
//                        && atomsHolds(otherTuple.state, reachableTuple.state)
//                        && recursiveBisimCheck(otherTuple.state, reachableTuple.state, markedStates)
//            }
//        }
//    }


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

data class StatePair(val state1: State, val state2: State){

    override fun equals(other: Any?): Boolean {
        return other is StatePair &&
                ((state1 == other.state1 && state2 == other.state2)
                        || (state1 == other.state2 && state2 == other.state1))
    }

    override fun hashCode(): Int {
        var result = state1.hashCode()
        result = 31 * result + state2.hashCode()
        return result
    }
}