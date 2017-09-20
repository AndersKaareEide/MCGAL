package formulaParser

import canvas.AgentItem
import canvas.State

/**
 * Based on an agent and a state, returns all states the given agent considers
 * indistinguishable from the given state
 */
fun getIndishStates(agent: AgentItem, state: State): List<State> {
    val outEdges = state.outEdges.filter { it.agents.contains(agent) }
    val inEdges = state.inEdges.filter { it.agents.contains(agent) }

    val outStates = outEdges.map { it.parent1 }
    val inStates = inEdges.map { it.parent2 }


    val list = inStates + outStates + state
    list.forEach{(println(it.name))}
    return list
}