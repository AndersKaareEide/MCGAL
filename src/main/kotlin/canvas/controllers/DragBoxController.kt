package canvas.controllers

import canvas.views.DragRectangle
import javafx.scene.input.MouseEvent
import tornadofx.*

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
        DragRectangle.y = it.sceneY
        DragRectangle.origX = it.sceneX
        DragRectangle.origY = it.sceneY
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
                y = it.sceneY
                height = origY - y
            } else {
                height = it.sceneY - y
            }
        }
    }

    //TODO Find way to trigger this even if the drag gesture ends off-screen
    fun handleCanvasDragEnd(){
        val bounds = DragRectangle.boundsInLocal

        controller.selectStates(bounds)
        DragRectangle.isVisible = false
    }


}