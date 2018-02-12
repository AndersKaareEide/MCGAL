package canvas.controllers

import canvas.data.AgentItem
import canvas.data.Edge
import canvas.data.Edge.Companion.makeEdgeBetween
import canvas.data.State
import formulafield.FormulaFieldController
import javafx.collections.FXCollections
import javafx.scene.input.MouseEvent
import sidepanels.agentpanel.AgentPanelController
import tornadofx.*
import utils.defaultEdges

class EdgeController: Controller() {

    private val agentController: AgentPanelController by inject()
    private val formulaController: FormulaFieldController by inject()

    val edges = FXCollections.observableArrayList(defaultEdges)!!
    val selectedEdges = FXCollections.observableSet<Edge>()!!

    fun addEdge(parent1: State, parent2: State) {
        formulaController.clearErrorMsg()

        val agents = agentController.getSelected()
        val newEdge = makeEdgeBetween(parent1, parent2, ArrayList(agents))

        if (edges.contains(newEdge)) {
            if (agents.isEmpty()){
                edges.remove(newEdge)
            } else {
                //Get reference to existing edge with same parents
                val oldEdge = edges[edges.indexOf(newEdge)]
                oldEdge.agents.setAll(agents)
            }
        } else if(!agents.isEmpty()) {
            edges.add(newEdge)
        } else {
            formulaController.setErrorMsg("Can't create edge without selecting agents first")
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

    fun selectEdge(item: Edge) {
        selectedEdges.add(item)
    }

    fun removeSelected() {
        selectedEdges.filter { it.isSelected }
                .forEach { edges.remove(it) }
    }

    fun clearSelected() {
        selectedEdges.clear()
    }

    fun setAgents(item: Edge, it: MouseEvent) {
        if (it.isControlDown) {
            item.agents.setAll(agentController.getSelected())
        }
    }
}