package canvas

import javafx.beans.property.SimpleBooleanProperty
import javafx.collections.FXCollections
import javafx.collections.ObservableList
import javafx.scene.Node
import javafx.scene.input.MouseEvent
import tornadofx.*

class CanvasController : Controller() {

    //TODO Remove, used for manual testing purposes only
    val state1 = State("s1", 150.0, 200.0, FXCollections.observableArrayList("p"))
    val state2 = State("s2", 50.0, 70.0, FXCollections.observableArrayList("p", "q"))

    val states = FXCollections.observableArrayList(state1, state2)
    val edges = FXCollections.observableArrayList(Edge(state1, state2, mutableListOf(AgentItem("a", true))))

    val canvas: Canvas by inject()

    val agentController: AgentPanelController by inject()
    val isDrawingLinesProperty = SimpleBooleanProperty(this, "isDrawingLines", false)

    val isDrawingLines by isDrawingLinesProperty
    var lastClickedState: State? = null

    var deltaX = 0.0
    var deltaY = 0.0

    val model = Model(states, edges, agentController.agents)

    fun handleMPress(item: State, event: MouseEvent){
        if (!isDrawingLines)
            setDragDelta(item, event)
    }

    private fun setDragDelta(item: State, event: MouseEvent){
        deltaX = item.xPos - event.sceneX
        deltaY = item.yPos - event.sceneY
    }

    fun handleMDrag(item: State, event: MouseEvent) {
        if (isDrawingLines.not())
            dragItem(item, event)
    }

    fun handleDragEnd(item: State){
        if (isDrawingLines && lastClickedState != null) {
            val agents = agentController.getSelected()
            if (!agents.isEmpty()) {
                addEdge(item, agents)
            } else {
                //TODO Provide visual feedback
                println("Can't create edge without selecting agents first")
            }
        }
    }

    private fun addEdge(item: State, agents: ObservableList<AgentItem>) {
        val newEdge = Edge(lastClickedState!!, item, ArrayList(agents))
        if (edges.contains(newEdge)){
            //Get reference to existing edge with same parents
            val oldEdge = edges[edges.indexOf(newEdge)]
            oldEdge.agents.setAll(agents)
        } else {
            edges.add(newEdge)
        }
    }

    private fun dragItem(item: State, event:MouseEvent){
        item.xPos = deltaX + event.sceneX
        item.yPos = deltaY + event.sceneY
    }

    fun startLineDrawing(item: State, node: Node) {
        if (isDrawingLines) {
            lastClickedState = item
            node.startFullDrag()
        }
    }

    fun handleCanvasClick(event: MouseEvent) {
        if (!isDrawingLines)
            addState(event)
    }

    private fun addState(event: MouseEvent) {
        //TODO Add props to state
        val posX = event.sceneX - STATE_CIRCLE_RADIUS
        val posY = event.sceneY - STATE_CIRCLE_RADIUS
        states.add(State("s${states.size + 1}", posX, posY))
    }
}