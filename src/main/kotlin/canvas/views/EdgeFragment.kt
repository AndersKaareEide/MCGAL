package canvas.views

import canvas.STATE_CIRCLE_RADIUS
import canvas.controllers.CanvasController
import canvas.controllers.EdgeController
import canvas.data.Edge
import canvas.styles.ModelStyles
import formulafield.FormulaFieldController
import javafx.geometry.Pos
import javafx.scene.Node
import javafx.scene.transform.Rotate
import tornadofx.*
import utils.getPositionBinding

class EdgeFragment(val item: Edge) : Fragment("My View") {

    private val formulaController: FormulaFieldController by inject()
    private val edgeController: EdgeController by inject()
    private val canvasController: CanvasController by inject()

    private val x1 = item.outParent.xProperty + STATE_CIRCLE_RADIUS
    private val y1 = item.outParent.yProperty + STATE_CIRCLE_RADIUS
    private val x2 = item.inParent.xProperty + STATE_CIRCLE_RADIUS
    private val y2 = item.inParent.yProperty + STATE_CIRCLE_RADIUS

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
            alignment = Pos.CENTER
            textProperty().bind(stringBinding(item.agentsProperty) {
                item.agentsProperty.value.joinToString(",") { it.name }
            })

            val labelPosBinding = getPositionBinding(x1, x2, y1, y2)

            translateXProperty().bind(labelPosBinding.doubleBinding(labelPosBinding){ labelPosBinding.value!!.xPos })
            translateYProperty().bind(labelPosBinding.doubleBinding(labelPosBinding){ labelPosBinding.value!!.yPos })

            val rotation = Rotate(0.0, 0.0, 0.0)
            rotation.angleProperty().bind(doubleBinding(labelPosBinding) {
                labelPosBinding.value!!.rotDegrees
            })

            transforms.add(rotation)
            addEdgeListeners(this)
        }
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
