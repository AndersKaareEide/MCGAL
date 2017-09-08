import javafx.beans.property.SimpleBooleanProperty
import javafx.scene.Node
import javafx.scene.input.MouseEvent
import tornadofx.*

class CanvasStateController : Controller() {

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

    fun setDragDelta(item: State, event: MouseEvent){
        deltaX = item.xPos - event.sceneX
        deltaY = item.yPos - event.sceneY
    }

    fun handleMDrag(item: State, event: MouseEvent) {
        if (isDrawingLines.not())
            dragItem(item, event)
    }

    fun handleDragEnd(item: State){
        if (isDrawingLines)
            println("Draw line from ${lastClickedState?.name} to ${item.name}")
    }

    fun dragItem(item: State, event:MouseEvent){
        item.xPos = deltaX + event.sceneX
        item.yPos = deltaY + event.sceneY
    }

    fun startLineDrawing(item: State, node: Node) {
        if (isDrawingLines)
            lastClickedState = item
            node.startFullDrag()
    }

}