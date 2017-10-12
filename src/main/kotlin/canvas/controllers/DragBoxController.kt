package canvas.controllers

import canvas.views.DragRectangle
import javafx.scene.input.MouseDragEvent
import javafx.scene.input.MouseEvent
import tornadofx.*

private const val MOUSE_OFFSET = 25

class DragBoxController : Controller() {

    val controller: CanvasController by inject()


    fun handleCanvasDragStart(it: MouseEvent) {
        if (controller.clickMode == ClickMode.MOVING){
            startDragSelection(it)
            DragRectangle.startFullDrag()
        }
    }

    private fun startDragSelection(it: MouseEvent) {
        DragRectangle.width = 0.0
        DragRectangle.height = 0.0
        DragRectangle.x = it.sceneX
        DragRectangle.y = it.sceneY - MOUSE_OFFSET
        DragRectangle.origX = it.sceneX
        DragRectangle.origY = it.sceneY - MOUSE_OFFSET
        DragRectangle.isVisible = true
    }

    fun handleCanvasDrag(it: MouseEvent) {
        with(DragRectangle) {
            val tempWidth = it.sceneX - origX
            val tempHeight = it.sceneY - origY

            if (tempWidth < 0){
                x = it.sceneX
                width = origX - x
            } else {
                x = origX
                width = it.sceneX - x
            }

            if (tempHeight < 0){
                y = it.sceneY - MOUSE_OFFSET
                height = origY - y
            } else {
                height = (it.sceneY - MOUSE_OFFSET) - y
            }
        }
    }

    //TODO Find way to trigger this even if the drag gesture ends off-screen
    fun handleCanvasDragEnd(it: MouseDragEvent) {
        if (DragRectangle.isVisible) {
            val bounds = DragRectangle.boundsInLocal

            controller.selectStates(bounds, it)
            DragRectangle.isVisible = false
        }
    }
}