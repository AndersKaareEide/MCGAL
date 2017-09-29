package canvas.views

import canvas.controllers.StateController
import javafx.collections.FXCollections
import javafx.collections.ObservableList
import javafx.event.EventTarget
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import tornadofx.*

fun EventTarget.dragrectangle(op: (DragRectangle.() -> Unit)? = null): DragRectangle  {
    return opcr(this, DragRectangle, op)
}

object DragRectangle : Rectangle() {

    var origX = 0.0
    var origY = 0.0

    init {
        height = 0.0
        width = 0.0
        isManaged = false
        isVisible = false

        fill = Color.TRANSPARENT
        stroke = Color.BLUE
        strokeWidth = 1.0
    }

    override fun toString(): String {
        return "$origX, $origY, $x, $y, $height, $width"
    }
}
