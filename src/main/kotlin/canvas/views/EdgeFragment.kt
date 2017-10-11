package canvas.views

import canvas.STATE_CIRCLE_RADIUS
import canvas.controllers.CanvasController
import canvas.controllers.EdgeController
import canvas.data.Edge
import canvas.styles.ModelStyles
import javafx.geometry.Pos
import javafx.scene.Node
import javafx.scene.input.KeyCode
import tornadofx.*

class EdgeFragment(val item: Edge) : Fragment("My View") {

    val edgeController: EdgeController by inject()
    val canvasController: CanvasController by inject()


    val x1 = item.parent1.xProperty
    val y1 = item.parent1.yProperty
    val x2 = item.parent2.xProperty
    val y2 = item.parent2.yProperty



    override val root = anchorpane {

        toggleClass(ModelStyles.hidden, item.hiddenProperty)
        toggleClass(ModelStyles.selected, item.selectedProperty)

        isPickOnBounds = false

        line {
            startXProperty().bind(x1 + STATE_CIRCLE_RADIUS)
            startYProperty().bind(y1 + STATE_CIRCLE_RADIUS)
            endXProperty().bind(x2 + STATE_CIRCLE_RADIUS)
            endYProperty().bind(y2 + STATE_CIRCLE_RADIUS)
        }

        label {
            //TODO Bind positioning to angle as well so it looks good at 90* angles and such
            //TODO Bind max size to distance between states and make it use multiple lines on large amounts of agents (or ...)
            alignment = Pos.CENTER
            textProperty().bind(stringBinding(item.agentsProperty) {
                item.agentsProperty.value.joinToString(",") { it.name }
            })

            translateXProperty().bind((((x1 + x2) / 2) - widthProperty() / 2) + STATE_CIRCLE_RADIUS)
            translateYProperty().bind((((y1 + y2) / 2) - heightProperty() / 2) + (STATE_CIRCLE_RADIUS - 10.0))

            rotateProperty().bind(doubleBinding(x1, y1, x2, y2) {
                getAngle(x1.value, y1.value, x2.value, y2.value)
            })

            addEdgeListeners(this)
        }

    }

    private fun addEdgeListeners(node: Node) {
        with(node){
            setOnMousePressed { canvasController.handleSelectionClick(it, item); it.consume() }
            setOnMouseClicked { edgeController.setAgents(item, it); it.consume() }
        }
    }

    /**
     * Function used to calculate the rotation of the label on edges
     */
    private fun getAngle(x1: Double, y1: Double, x2: Double, y2: Double): Double{
        val difX = x1 - x2; val difY = y1 - y2
        var degrees = Math.toDegrees(Math.atan2(difY, difX)) - 180.0
        if (Math.abs(degrees) > 90 && Math.abs(degrees) < 270)
            degrees -= 180

        return degrees
    }
}
