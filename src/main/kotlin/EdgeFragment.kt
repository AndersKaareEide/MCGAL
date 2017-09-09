import javafx.beans.property.DoubleProperty
import javafx.geometry.Pos
import tornadofx.*

class EdgeFragment(val item: Edge) : Fragment("My View") {

    override val root = anchorpane {
        val x1: DoubleProperty = item.parent1.xProperty
        val y1 = item.parent1.yProperty
        val x2 = item.parent2.xProperty
        val y2 = item.parent2.yProperty

        line {
            startXProperty().bind(x1 + STATE_CIRCLE_RADIUS)
            startYProperty().bind(y1 + STATE_CIRCLE_RADIUS)
            endXProperty().bind(x2 + STATE_CIRCLE_RADIUS)
            endYProperty().bind(y2 + STATE_CIRCLE_RADIUS)
        }

        label {
            //TODO Bind positioning to angle as well so it looks good at 90* angles and such
            alignment = Pos.CENTER
            textProperty().bind(item.agentsProperty)

            translateXProperty().bind((((x1 + x2) / 2) - widthProperty() / 2) + STATE_CIRCLE_RADIUS)
            translateYProperty().bind((((y1 + y2) / 2) - heightProperty() / 2) + (STATE_CIRCLE_RADIUS - 10.0))

            rotateProperty().bind(doubleBinding(x1, y1, x2, y2) {
                getAngle(x1.value, y1.value, x2.value, y2.value)
            })
        }

    }

    /**
     * Function used to calculate the rotation of the label on edges
     */
    fun getAngle(x1: Double, y1: Double, x2: Double, y2: Double): Double{
        val difX = x1 - x2; val difY = y1 - y2
        var degrees = Math.toDegrees(Math.atan2(difY, difX)) - 180.0
        if (Math.abs(degrees) > 90 && Math.abs(degrees) < 270)
            degrees -= 180

        return degrees
    }
}
