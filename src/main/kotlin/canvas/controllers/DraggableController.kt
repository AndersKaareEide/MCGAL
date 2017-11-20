package canvas.controllers

import canvas.data.State
import canvas.views.Draggable
import javafx.collections.ObservableSet
import javafx.scene.input.MouseEvent
import tornadofx.*

class DraggableController : Controller() {

    private var offsetX = 0.0
    private var offsetY = 0.0

    fun setDragOffset(item: Draggable, event: MouseEvent){
        offsetX = item.xPos - event.sceneX
        offsetY = item.yPos - event.sceneY
    }

    fun dragItem(item: Draggable, event: MouseEvent){
        item.xPos = offsetX + event.sceneX
        item.yPos = offsetY + event.sceneY
    }

    fun dragGroup(draggedState: State, group: ObservableSet<State>, event: MouseEvent) {
        val newX = offsetX + event.sceneX
        val newY = offsetY + event.sceneY

        group.forEach {
            if (it != draggedState) {
                it.xPos = newX + (it.xPos - draggedState.xPos)
                it.yPos = newY + (it.yPos - draggedState.yPos)
            }
        }
        draggedState.yPos = newY
        draggedState.xPos = newX
    }
}