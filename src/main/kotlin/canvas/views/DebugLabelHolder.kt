package canvas.views

import canvas.STATE_CIRCLE_RADIUS
import canvas.controllers.DraggableController
import canvas.data.State
import javafx.beans.property.SimpleDoubleProperty
import sidepanels.debugpanel.DebugLabel
import tornadofx.*

val dragController = find(DraggableController::class)

class DebugLabelHolder(val state: State) : Fragment("My View"), Draggable {

    override val xProperty = SimpleDoubleProperty(this, "xPos", (STATE_CIRCLE_RADIUS * 2) + 5)
    override var xPos by xProperty

    override val yProperty = SimpleDoubleProperty(this, "yPos", 0.0)
    override var yPos by yProperty

    override val root = borderpane {
        translateXProperty().bind(xProperty + state.xProperty)
        translateYProperty().bind(yProperty + state.yProperty)

        isPickOnBounds = false

        right = vbox {
            visibleProperty().bind(booleanBinding(state.debugLabelsProperty){
                state.debugLabelsProperty.isNotEmpty() && state.debugLabelsProperty[0].isNotEmpty()
            })

            bindChildren(state.debugLabelsProperty) {
                hbox {
                    bindChildren(it) { label ->
                        val debugLabel = DebugLabel(label)
                        addMouseListeners(debugLabel)
                        debugLabel
                    }
                }
            }
        }
    }

    private fun addMouseListeners(debugLabel: DebugLabel) {
        debugLabel.setOnMousePressed { dragController.setDragOffset(this, it); it.consume() }
        debugLabel.setOnDragDetected { debugLabel.startFullDrag(); it.consume() }
        debugLabel.setOnMouseDragged { dragController.dragItem(this, it) }
    }
}
