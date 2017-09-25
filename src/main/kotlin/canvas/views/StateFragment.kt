package canvas.views

import canvas.STATE_CIRCLE_RADIUS
import canvas.controllers.CanvasController
import canvas.data.State
import canvas.styles.StateStyles
import javafx.beans.value.WeakChangeListener
import javafx.geometry.Pos
import javafx.scene.input.KeyCode
import javafx.scene.paint.Color
import tornadofx.*


class StateFragment(val item: State) : Fragment() {
    val controller = find(CanvasController::class)

    override val root =
            borderpane {
                translateXProperty().bind(item.xProperty)
                translateYProperty().bind(item.yProperty)

                toggleClass(StateStyles.hidden, item.hiddenProperty)

                center = stackpane {
                    circle {
                        radius = STATE_CIRCLE_RADIUS
                        fill = Color.WHITE

                        toggleClass(StateStyles.focused, focusedProperty())
                        focusedProperty().addListener(WeakChangeListener {
                            _, _, newValue ->
                            when (newValue) {
                                true -> this@circle.setOnKeyPressed {
                                    if (it.code == KeyCode.DELETE){
                                        controller.removeState(this@StateFragment)
                                    }
                                }
                                false -> this@circle.setOnKeyPressed {}
                            }
                        })

                        setOnMousePressed { controller.handleMPress(item, it); it.consume() }
                        setOnDragDetected { controller.startLineDrawing(item, this); it.consume() }
                        setOnMouseDragged { controller.handleMDrag(item, it); it.consume() }
                        setOnMouseDragReleased { controller.handleDragEnd(item); it.consume() }
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
                }
            }
}

