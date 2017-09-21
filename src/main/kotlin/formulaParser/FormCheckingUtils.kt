package formulaParser

import canvas.AgentItem
import canvas.Model
import canvas.State

/**
 * Based on an agent and a state, returns all states the given agent considers
 * indistinguishable from the given state
 */
fun getIndishStates(agent: AgentItem, state: State, model: Model): List<State> {
    val outEdges = state.outEdges.filter { it.agents.contains(agent) && model.edges.contains(it) }
    val inEdges = state.inEdges.filter { it.agents.contains(agent) && model.edges.contains(it) }

    val outStates = outEdges.map { it.parent1 }
    val inStates = inEdges.map { it.parent2 }

    return inStates + outStates + state
}

fun updateModel(announcement: Formula, model: Model): Model {
    val updStates = model.states.filter { announcement.check(it, model) }
    val updEdges = model.edges.filter { (updStates.contains(it.parent1) and updStates.contains(it.parent2)) }

    return Model(updStates, updEdges, model.agents)
}

/**
 * Extracts all the propositions the input formula is built on
 */
fun extractProps(formula: Formula): Set<String> {
    return when (formula){
        is Proposition -> setOf(formula.propString)
        is Negation -> extractProps(formula.inner)
        is BinaryOperator -> extractProps(formula.left) + extractProps(formula.right)
        is Knows -> extractProps(formula.inner)
        is Announcement -> extractProps(formula.inner)
        is GroupAnn -> extractProps(formula.inner)
        else -> throw RuntimeException("Missing branch in extractProps for ${formula.javaClass}")
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

    return Model(model.states, filteredEdges, model.agents)
}