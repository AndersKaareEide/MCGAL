package formulaParser

import canvas.AgentItem
import canvas.Model
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

    return inStates + outStates + state
}

fun updateModel(announcement: Formula, model: Model): Model {
    val updStates = model.states.filter { announcement.check(it, model) }
    val updEdges = model.edges.filter { (updStates.contains(it.parent1) and updStates.contains(it.parent2)) }

    return Model(updStates, updEdges, model.agents)
}