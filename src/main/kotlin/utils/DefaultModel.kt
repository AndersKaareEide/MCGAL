package utils

import canvas.data.AgentItem
import canvas.data.Edge
import canvas.data.State
import javafx.collections.FXCollections
import sidepanels.propertypanel.PropositionItem

private val propP = PropositionItem("p", true)
private val propQ = PropositionItem("q", false)

private val agentA = AgentItem("a", true)
private val agentB = AgentItem("b", false)
private val agentC = AgentItem("c", false)

private val state1 = State("s1", 150.0, 200.0, FXCollections.observableArrayList(propP))
private val state2 = State("s2", 50.0, 70.0, FXCollections.observableArrayList(propP, propQ))

private val edgeS1S2 = Edge(state1, state2, mutableListOf(agentA))

val defaultStates = listOf(state1, state2)
val defaultEdges = listOf(edgeS1S2)
val defaultAgents = listOf(agentA, agentB, agentC)
val defaultProps = listOf(propP, propQ)


