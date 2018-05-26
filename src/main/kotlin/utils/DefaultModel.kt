package utils

import canvas.data.AgentItem
import canvas.data.Edge.Companion.makeEdgeBetween
import canvas.data.Model
import canvas.data.State
import javafx.collections.FXCollections
import sidepanels.propertypanel.PropositionItem

private val propP = PropositionItem("regn", true)

private val agentA = AgentItem("Arne", true)
private val agentB = AgentItem("Bjarne", false)

private val state1 = State("s1", 250.0, 300.0, FXCollections.observableArrayList(propP))
private val state2 = State("s2", 150.0, 170.0, FXCollections.observableArrayList())

private val edgeS1S2 = makeEdgeBetween(state1, state2, mutableListOf(agentA))

private val defaultStates = listOf(state1, state2)
private val defaultEdges = listOf(edgeS1S2)
private val defaultAgents = listOf(agentA, agentB)
private val defaultProps = listOf(propP)

fun getModel() : Model = Model(defaultStates, defaultEdges, defaultAgents, defaultProps)


