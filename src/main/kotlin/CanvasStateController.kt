import javafx.beans.property.SimpleBooleanProperty
import tornadofx.*

class CanvasStateController : Controller() {

    val isDrawingLinesProperty = SimpleBooleanProperty(this, "isDrawingLines", false)
    var isDrawingLines by isDrawingLinesProperty
    var lastClickedState: State? = null

    var deltaX = 0.0
    var deltaY = 0.0

    fun handleMPress(item: State, sceneX: Double, sceneY: Double){
        if (isDrawingLines)
            lastClickedState = item
        else
            startDrag(item, sceneX, sceneY)
    }

    fun handleMDrag(item: State, sceneX: Double, sceneY: Double) {
        if (isDrawingLines)
            dragItem(item, sceneX, sceneY)
    }

    fun handleDragEnd(item: State?){
        if (item != null){

        }
    }

    fun startDrag(item: State, xPos: Double, yPos: Double){
        deltaX = item.xPos - xPos
        deltaY = item.yPos - yPos
    }

    fun dragItem(item: State, xPos: Double, yPos: Double){
        item.xPos = deltaX + xPos
        item.yPos = deltaY + yPos
    }

}