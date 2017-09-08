import javafx.geometry.Pos
import javafx.scene.paint.Color
import tornadofx.*

class StateView(val item: State) : Fragment() {
    val controller = find(CanvasStateController::class)

    override val root =
            borderpane {
                translateXProperty().bind(item.xProperty)
                translateYProperty().bind(item.yProperty)

                setOnMousePressed { controller.handleMPress(item, it.sceneX, it.sceneY) }
                setOnMouseDragged { controller.handleMDrag(item, it.sceneX, it.sceneY) }
                setOnMouseDragReleased { controller.handleDragEnd(item) }

                center = stackpane {
                    circle {
                        radius = 25.0
                        fill = Color.WHITE
                    }
                    label { textProperty().bind(item.nameProperty) }
                }

                bottom = label {
                    textProperty().bind(item.propsProperty)
                    useMaxWidth = true
                    alignment = Pos.CENTER
//                    border = Border(BorderStroke(Color.BLACK, BorderStrokeStyle.SOLID, CornerRadii.EMPTY, BorderWidths.DEFAULT))
//                    For debugging purposes
                }
            }
}

