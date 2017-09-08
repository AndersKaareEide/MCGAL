import javafx.beans.property.DoubleProperty
import javafx.scene.text.TextAlignment
import tornadofx.*

class EdgeFragment(val item: Edge) : Fragment("My View") {

    override val root = anchorpane {
        val x1: DoubleProperty = item.parent1.xProperty
        val y1 = item.parent1.yProperty
        val x2 = item.parent2.xProperty
        val y2 = item.parent2.yProperty

        line {
            //TODO Bind circleRadius to actual radius of circles in StateFragment
            val circleRadius = 25.0
            startXProperty().bind(x1 + circleRadius)
            startYProperty().bind(y1 + circleRadius)
            endXProperty().bind(x2 + circleRadius)
            endYProperty().bind(y2 + circleRadius)
        }
        label("List of agents goes here") {
            translateXProperty().bind((x1 + x2) / 2)
            translateYProperty().bind((y1 + y2) / 2)
            rotateProperty().bind(doubleBinding(x1, y1, x2, y2) {
                getAngle(x1.value, y1.value, x2.value, y2.value)
            })
        }

    }

    fun getAngle(x1: Double, y1: Double, x2: Double, y2: Double): Double{
        //TODO Fix so label is never upside-down
        val difX = x1 - x2; val difY = y1 - y2
        var degrees = Math.toDegrees(Math.atan2(difY, difX))
        return degrees
    }
}
