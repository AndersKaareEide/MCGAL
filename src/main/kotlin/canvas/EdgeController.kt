package canvas

import tornadofx.*

class EdgeController: Controller() {
    val canvas: Canvas by inject()

    fun removeEdge(edge: Edge) {
        canvas.edges.remove(edge)
    }

    fun removeAgent(item: AgentItem) {
        canvas.edges.forEach {
            it.agents.remove(item)
        }
        //Remove 'empty' edges after removing agent from edges
        canvas.edges.removeIf {
            it.agents.isEmpty()
        }
    }
}