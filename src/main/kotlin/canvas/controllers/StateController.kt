package canvas.controllers

import canvas.STATE_CIRCLE_RADIUS
import canvas.data.State
import canvas.views.Canvas
import canvas.views.StateFragment
import javafx.beans.property.SimpleBooleanProperty
import javafx.collections.FXCollections
import javafx.scene.Node
import javafx.scene.input.MouseEvent
import sidepanels.agentpanel.AgentPanelController
import sidepanels.propertypanel.PropPanelController
import tornadofx.*
import utils.defaultStates

class StateController : Controller() {

    val propController: PropPanelController by inject()
    val edgeController: EdgeController by inject()

    var states = FXCollections.observableArrayList(defaultStates)!!

    val canvas: Canvas by inject()

    val isDrawingLinesProperty = SimpleBooleanProperty(this, "isDrawingLines", false)

    val isDrawingLines by isDrawingLinesProperty
    var lastClickedState: State? = null

    var deltaX = 0.0
    var deltaY = 0.0


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
            edgeController.addEdge(lastClickedState!!, item)
        }
    }

    private fun dragItem(item: State, event: MouseEvent){
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
        val posX = event.sceneX - STATE_CIRCLE_RADIUS
        val posY = event.sceneY - STATE_CIRCLE_RADIUS
        states.add(State("s${states.size + 1}", posX, posY, propController.getSelected()))
    }

    //TODO Find out if this potentially leaks memory due to loose references
    fun removeState(stateFragment: StateFragment) {
        val state = stateFragment.item
        for (edge in state.inEdges + state.outEdges){
            edgeController.removeEdge(edge)
        }
        states.remove(state)
        stateFragment.close()
    }

}
