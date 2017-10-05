package canvas.views

import canvas.STATE_CIRCLE_RADIUS
import canvas.controllers.CanvasController
import canvas.controllers.StateController
import canvas.data.State
import canvas.styles.ModelStyles
import javafx.geometry.Pos
import javafx.scene.input.KeyCode
import javafx.scene.paint.Color
import tornadofx.*


class StateFragment(val item: State) : Fragment() {
    val controller: StateController by inject()
    val canvasController: CanvasController by inject()

    //TODO Use double-click to set properties or something?
    override val root =
            borderpane {
                translateXProperty().bind(item.xProperty)
                translateYProperty().bind(item.yProperty)

                toggleClass(ModelStyles.hidden, item.hiddenProperty)
                toggleClass(ModelStyles.selected, item.selectedProperty)

                center = stackpane {
                    circle {
                        radius = STATE_CIRCLE_RADIUS
                        fill = Color.WHITE

                        setOnMousePressed {
                            controller.handleStateMPress(item, it)
                            canvasController.handleSelectionClick(it, item)
                            it.consume()
                        }
                        //TODO Make it so that you don't have to hold shift while initiating drag to drag multiple states
                        setOnDragDetected { controller.startLineDrawing(item, this); it.consume() }
                        setOnMouseDragged { controller.handleMDrag(item, it); it.consume() }
                        setOnMouseDragReleased { controller.handleDragEnd(item, it) }
                        setOnMouseClicked { it.consume() } //Prevent from bubbling up to Canvas and triggering addState()

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
}

