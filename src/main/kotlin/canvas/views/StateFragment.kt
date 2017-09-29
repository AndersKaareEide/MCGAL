package canvas.views

import canvas.STATE_CIRCLE_RADIUS
import canvas.controllers.StateController
import canvas.data.State
import canvas.styles.ModelStyles
import javafx.geometry.Pos
import javafx.scene.input.KeyCode
import javafx.scene.paint.Color
import tornadofx.*


class StateFragment(val item: State) : Fragment() {
    val controller = find(StateController::class)

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

                        item.selectedProperty.addListener({
                            _, _, newValue ->
                            when (newValue) {
                                true -> this@circle.setOnKeyPressed {
                                    if (it.code == KeyCode.DELETE){
                                        controller.removeSelected()
                                    }
                                }
                                false -> this@circle.setOnKeyPressed {}
                            }
                        })

                        setOnMousePressed { controller.handleStateMPress(item, it); it.consume() }
                        setOnDragDetected { controller.startLineDrawing(item, this); it.consume() }
                        setOnMouseDragged { controller.handleMDrag(item, it); it.consume() }
                        setOnMouseDragReleased { controller.handleDragEnd(item, it) }
                        setOnMouseClicked { this@circle.requestFocus(); it.consume() }

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
                    //TODO Find out why mouse events get eaten by this label
                    isMouseTransparent = true
                }
            }
}

