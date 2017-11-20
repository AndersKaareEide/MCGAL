package canvas.views

import canvas.STATE_CIRCLE_RADIUS
import canvas.controllers.CanvasController
import canvas.controllers.DraggableController
import canvas.controllers.StateController
import canvas.data.State
import canvas.styles.ModelStyles
import formulafield.FormulaFieldController
import javafx.geometry.Pos
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
import tornadofx.*


class StateFragment(val item: State) : Fragment() {
    private val controller: StateController by inject()
    private val canvasController: CanvasController by inject()
    private val formulaController: FormulaFieldController by inject()

    //TODO Use double-click to set properties or something?
    override val root =
            borderpane {
                translateXProperty().bind(item.xProperty)
                translateYProperty().bind(item.yProperty)

                center = stackpane {
                    circle {
                        toggleClass(ModelStyles.selected, item.selectedProperty)
                        bindClass(item.validationStyleProp)

                        radius = STATE_CIRCLE_RADIUS
                        fill = Color.WHITE

                        addMouseListeners()
                    }
                    label {
                        textProperty().bind(item.nameProperty)
                        isMouseTransparent = true
                    }
                }

                bottom = label {
                    textProperty().bind(stringBinding(item.propsProperty) {
                        item.propsProperty.value.joinToString(","){ it.propString }
                    })
                    useMaxWidth = true
                    alignment = Pos.CENTER
                    isMouseTransparent = true
                }
            }

    private fun Circle.addMouseListeners() {
        setOnMousePressed {
            controller.handleStateMPress(item, it)
            canvasController.handleSelectionMPress(it, item)
            formulaController.clearValidation()
            it.consume()
        }
        setOnDragDetected {
            startFullDrag()
            controller.startLineDrawing(item)
            canvasController.isDragging = true
            it.consume()
        }
        setOnMouseDragged { controller.handleMDrag(item, it); it.consume() }
        setOnMouseDragReleased {
            controller.handleDragEnd(item, it)
            it.consume()
        }
        setOnMouseClicked {
            canvasController.handleSelectionClick(it, item)
            it.consume() //Prevent from bubbling up to Canvas and triggering addState()
        }
    }
}

