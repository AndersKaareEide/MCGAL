package canvas

import tornadofx.*

class EdgeController: Controller() {
    val canvasController: CanvasController by inject()

    fun removeEdge(edge: Edge) {
        canvasController.edges.remove(edge)
    }

    fun removeAgent(item: AgentItem) {
        canvasController.edges.forEach {
            it.agents.remove(item)
        }
        //Remove 'empty' edges after removing agent from edges
        canvasController.edges.removeIf {
            it.agents.isEmpty()
        }
    }
}