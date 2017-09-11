package canvas

import javafx.geometry.Pos
import javafx.scene.paint.Color
import tornadofx.*


class StateFragment(val item: State) : Fragment() {
    val controller = find(CanvasStateController::class)

    override val root =
            borderpane {
                translateXProperty().bind(item.xProperty)
                translateYProperty().bind(item.yProperty)

                center = stackpane {
                    circle {
                        radius = STATE_CIRCLE_RADIUS
                        fill = Color.WHITE

                        setOnMousePressed { controller.handleMPress(item, it); it.consume() }
                        setOnDragDetected { controller.startLineDrawing(item, this); it.consume() }
                        setOnMouseDragged { controller.handleMDrag(item, it); it.consume() }
                        setOnMouseDragReleased { controller.handleDragEnd(item); it.consume() }
                        setOnMouseClicked { it.consume() } // Only used to stop events from bubbling upwards to the Canvas

                    }
                    label { textProperty().bind(item.nameProperty) }
                }

                bottom = label {
                    textProperty().bind(item.propsProperty)
                    useMaxWidth = true
                    alignment = Pos.CENTER
                }
            }
}

