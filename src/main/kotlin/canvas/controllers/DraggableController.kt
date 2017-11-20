package canvas.controllers

import canvas.views.DebugLabelHolder
import canvas.views.Draggable
import canvas.data.State
import javafx.scene.input.MouseEvent
import tornadofx.*

class DraggableController : Controller() {

    val canvasController = find(CanvasController::class)

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
}