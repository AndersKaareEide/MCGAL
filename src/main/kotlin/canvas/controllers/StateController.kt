package canvas.controllers

import canvas.STATE_CIRCLE_RADIUS
import canvas.data.State
import canvas.views.Canvas
import canvas.views.DragRectangle
import javafx.beans.binding.BooleanBinding
import javafx.beans.property.SimpleObjectProperty
import javafx.collections.FXCollections
import javafx.geometry.Point2D
import javafx.scene.Node
import javafx.scene.input.MouseEvent
import sidepanels.propertypanel.PropPanelController
import tornadofx.*
import utils.defaultStates

class StateController : Controller() {

    val propController: PropPanelController by inject()
    val edgeController: EdgeController by inject()

    var states = FXCollections.observableArrayList(defaultStates)!!
    val selectedStates = FXCollections.observableSet<State>()

    val canvas: Canvas by inject()

    val clickModeProperty = SimpleObjectProperty<ClickMode>(this, "clickMode", ClickMode.MOVING)
    var clickMode by clickModeProperty

    var lastClickedState: State? = null

    var deltaX = 0.0
    var deltaY = 0.0

    fun handleStateMPress(item: State, event: MouseEvent){
        if (!event.isShiftDown){
            selectedStates.forEach { it.isSelected = false }
            selectedStates.clear()
        }
        selectedStates.add(item)
        item.isSelected = true

        if (clickMode == ClickMode.MOVING) {
            setDragDelta(item, event)
        }
    }

    private fun setDragDelta(item: State, event: MouseEvent){
        deltaX = item.xPos - event.sceneX
        deltaY = item.yPos - event.sceneY
    }

    fun handleMDrag(state: State, event: MouseEvent) {
        if (clickMode == ClickMode.MOVING){
            dragItem(state, event)
        }
    }

    fun handleDragEnd(item: State, event: MouseEvent){
        if (clickMode == ClickMode.LINES && lastClickedState != null) {
            edgeController.addEdge(lastClickedState!!, item)
            event.consume()
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
        if (clickMode == ClickMode.LINES) {
            lastClickedState = item
            node.startFullDrag()
        }
    }

    fun handleCanvasClick(event: MouseEvent) {
        if (clickMode == ClickMode.STATES)
            addState(event)
    }

    //TODO Make state not positioned at mouse tip
    private fun addState(event: MouseEvent) {
        val posX = event.sceneX - STATE_CIRCLE_RADIUS
        val posY = event.sceneY - STATE_CIRCLE_RADIUS
        states.add(State(getNextStateID(), posX, posY, propController.getSelected()))
    }

    //TODO Find out if this potentially leaks memory due to loose references
    fun removeSelectedStates() {
        selectedStates.iterator().forEach {
            states.remove(it)

            for (edge in it.inEdges + it.outEdges) {
                edgeController.removeEdge(edge)
            }
        }

        selectedStates.clear()
    }

    private fun getNextStateID(): String {
        //TODO Find better solution this shit is retardedly slow
        outer@ for (stateNum in 1 until states.lastIndex + 3) {
            for (state in states){
                if (state.name == "s" + stateNum){
                    continue@outer
                }
            }
            return "s" + stateNum
        }
        throw RuntimeException("Failed to get next state id")
    }

    fun selectStates(states: List<State>, it: MouseEvent){
        if (!it.isShiftDown){
            selectedStates.forEach { it.isSelected = false }
            selectedStates.clear()
        }

        states.forEach { it.isSelected = true }
        selectedStates.addAll(states)
    }
}
