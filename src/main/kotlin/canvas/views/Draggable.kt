package canvas.views

import javafx.beans.property.SimpleDoubleProperty
import sun.java2d.pipe.SpanShapeRenderer
import tornadofx.*

interface Draggable {

    val xProperty : SimpleDoubleProperty
    var xPos : Double

    val yProperty : SimpleDoubleProperty
    var yPos : Double
}