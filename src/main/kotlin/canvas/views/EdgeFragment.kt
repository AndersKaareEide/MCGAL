package canvas.views

import canvas.STATE_CIRCLE_RADIUS
import canvas.controllers.CanvasController
import canvas.controllers.EdgeController
import canvas.data.Edge
import canvas.styles.ModelStyles
import formulafield.FormulaFieldController
import javafx.geometry.Pos
import javafx.scene.Node
import tornadofx.*

class EdgeFragment(val item: Edge) : Fragment("My View") {

    private val formulaController: FormulaFieldController by inject()
    private val edgeController: EdgeController by inject()
    private val canvasController: CanvasController by inject()

    private val x1 = item.parent1.xProperty + STATE_CIRCLE_RADIUS
    private val y1 = item.parent1.yProperty + STATE_CIRCLE_RADIUS
    private val x2 = item.parent2.xProperty + STATE_CIRCLE_RADIUS
    private val y2 = item.parent2.yProperty + STATE_CIRCLE_RADIUS

    override val root = anchorpane {

        toggleClass(ModelStyles.hidden, item.hiddenProperty)
        toggleClass(ModelStyles.selected, item.selectedProperty)

        isPickOnBounds = false

        line {
            startXProperty().bind(x1)
            startYProperty().bind(y1)
            endXProperty().bind(x2)
            endYProperty().bind(y2)
        }

        label {
            //TODO Bind positioning to angle as well so it looks good at 90* angles and such
            //TODO Bind max size to distance between states and make it use multiple lines on large amounts of agents (or ...)
            alignment = Pos.CENTER
            textProperty().bind(stringBinding(item.agentsProperty) {
                item.agentsProperty.value.joinToString(",") { it.name }
            })

            translateXProperty().bind(((x1 + x2) / 2) - widthProperty() / 2)
            translateYProperty().bind((((y1 + y2) / 2) - heightProperty() / 2) - 10)

            rotateProperty().bind(doubleBinding(x1, y1, x2, y2) {
                getLabelAngle(x1.value, y1.value, x2.value, y2.value)
            })

            addEdgeListeners(this)
        }

    }

    /**
     * Function used to calculate the rotation of the label on edges
     */
    private fun getLabelAngle(x1: Double, y1: Double, x2: Double, y2: Double): Double{
        val difX = x1 - x2; val difY = y1 - y2
        var degrees = Math.toDegrees(Math.atan2(difY, difX)) - 180.0
        if (Math.abs(degrees) > 90 && Math.abs(degrees) < 270)
            degrees -= 180

        return degrees
    }

    private fun addEdgeListeners(node: Node) {
        with(node){
            setOnMousePressed {
                canvasController.handleSelectionClick(it, item)
                formulaController.clearValidation()
                it.consume()
            }
            setOnMouseClicked { edgeController.setAgents(item, it); it.consume() }
        }
    }
}
