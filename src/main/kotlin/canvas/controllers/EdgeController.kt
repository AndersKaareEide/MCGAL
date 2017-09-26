package canvas.controllers

import canvas.data.AgentItem
import canvas.data.Edge
import canvas.data.State
import javafx.collections.FXCollections
import sidepanels.agentpanel.AgentPanelController
import tornadofx.*
import utils.defaultEdges

class EdgeController: Controller() {

    val agentController: AgentPanelController by inject()

    var edges = FXCollections.observableArrayList(defaultEdges)!!

    fun addEdge(parent1: State, parent2: State) {
        val agents = agentController.getSelected()
        if (!agents.isEmpty()) {
            val newEdge = Edge(parent1, parent2, ArrayList(agents))
            if (edges.contains(newEdge)) {
                //Get reference to existing edge with same parents
                val oldEdge = edges[edges.indexOf(newEdge)]
                oldEdge.agents.setAll(agents)
            } else {
                edges.add(newEdge)
            }
        } else {
            //TODO Provide visual feedback
            println("Can't create edge without selecting agents first")
        }
    }

    fun removeEdge(edge: Edge) {
        edges.remove(edge)
    }

    fun removeAgent(item: AgentItem) {
        edges.forEach {
            it.agents.remove(item)
        }
        //Remove 'empty' edges after removing agent from edges
        edges.removeIf {
            it.agents.isEmpty()
        }
    }
}