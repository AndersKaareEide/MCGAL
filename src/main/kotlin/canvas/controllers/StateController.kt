package canvas.controllers

import canvas.STATE_CIRCLE_RADIUS
import canvas.data.State
import canvas.views.Canvas
import javafx.collections.FXCollections
import javafx.geometry.Bounds
import javafx.geometry.Point2D
import javafx.scene.Node
import javafx.scene.input.MouseEvent
import sidepanels.propertypanel.PropPanelController
import tornadofx.*
import utils.defaultStates

class StateController : Controller() {

    val propController: PropPanelController by inject()
    val edgeController: EdgeController by inject()
    val canvasController: CanvasController by inject()

    val states = FXCollections.observableArrayList(defaultStates)!!
    val selectedStates = FXCollections.observableSet<State>()

    val canvas: Canvas by inject()

    var lastClickedState: State? = null

    var deltaX = 0.0
    var deltaY = 0.0

    fun handleStateMPress(item: State, event: MouseEvent){
        if (event.isControlDown){
            item.props.setAll(propController.getSelected())
        }
        if (canvasController.clickMode == ClickMode.MOVING) {
            setDragDelta(item, event)
        }
    }

    private fun setDragDelta(item: State, event: MouseEvent){
        deltaX = item.xPos - event.sceneX
        deltaY = item.yPos - event.sceneY
    }

    fun handleMDrag(state: State, event: MouseEvent) {
        if (canvasController.clickMode == ClickMode.MOVING){
            dragItem(state, event)
        }
    }

    fun handleDragEnd(item: State, event: MouseEvent){
        if (canvasController.clickMode == ClickMode.LINES && lastClickedState != null) {
            edgeController.addEdge(lastClickedState!!, item)
            event.consume()
        } else if (canvasController.clickMode == ClickMode.MOVING){
            recenterSelectedStates()
        }
    }

    //TODO Change it so that relative distance between states is maintained
    fun recenterSelectedStates(){
        val wHeight = canvas.currentWindow!!.height - 100
        val wWidth  = canvas.currentWindow!!.width - (240 + STATE_CIRCLE_RADIUS)

        selectedStates.forEach {
            if (it.xPos < 0) {
                it.xPos = 0.0
            } else if (it.xPos > wWidth){
                it.xPos = wWidth
            }

            if (it.yPos < 0) {
                it.yPos = 0.0
            } else if (it.yPos > wHeight - 55.0) {
                it.yPos = wHeight - 55.0
            }
        }
    }

    private fun dragItem(state: State, event: MouseEvent){
        val newX = deltaX + event.sceneX
        val newY = deltaY + event.sceneY

        selectedStates.forEach {
            if (it != state) {
                it.xPos = newX + (it.xPos - state.xPos)
                it.yPos = newY + (it.yPos - state.yPos)
            }
        }

        state.yPos = newY
        state.xPos = newX
    }

    fun startLineDrawing(item: State, node: Node) {
        node.startFullDrag()
        if (canvasController.clickMode == ClickMode.LINES) {
            lastClickedState = item
        }
    }

    fun addState(event: MouseEvent) {
        val posX = event.sceneX - STATE_CIRCLE_RADIUS
        val posY = event.sceneY - STATE_CIRCLE_RADIUS * 2
        states.add(State(getNextStateID(), posX, posY, ArrayList(propController.getSelected())))
    }

    fun removeSelected() {
        selectedStates.iterator().forEach {
            states.remove(it)

            for (edge in it.inEdges + it.outEdges) {
                edgeController.removeEdge(edge)
            }
        }

        selectedStates.clear()
    }

    fun selectState(state: State){
        selectedStates.add(state)
    }

    fun selectFromBounds(bounds: Bounds){
        val selected = states.filter {
            bounds.contains(Point2D(it.xPos + STATE_CIRCLE_RADIUS, it.yPos + STATE_CIRCLE_RADIUS))
        }
        selected.forEach { it.isSelected = true }
        selectedStates.addAll(selected)
    }

    fun selectAll(){
        selectedStates.clear()
        states.forEach {
            it.isSelected = true
            selectedStates.add(it)
        }
    }

    fun getNextStateID(): String {
        outer@ for (stateNum in 1 until states.lastIndex + 4) {
            for (state in states){
                if (state.name == "s" + stateNum){
                    continue@outer
                }
            }
            return "s" + stateNum
        }
        throw RuntimeException("Failed to get next state id")
    }

    fun clearSelected() {
        selectedStates.clear()
    }
}
