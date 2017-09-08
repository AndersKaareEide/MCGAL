import javafx.collections.FXCollections
import tornadofx.*

class Canvas : View("My View") {

    val controller: CanvasStateController by inject()
    val states = FXCollections.observableArrayList(
            State("s1", 0.0, 0.0),
            State("s2", 50.0, 70.0, "p, q"))

    override val root = borderpane {
        prefHeight = 600.0
        prefWidth = 600.0

//        setOnMouseClicked { controller.handle }

        center = anchorpane {
            bindChildren(states) {
                StateView(it).root
            }
//            setOnMouseDragReleased { controller.handleDragEnd(null) }
        }

        bottom = hbox {
            checkbox {
                controller.isDrawingStatesProperty.bind(selectedProperty())
            }
            label("Line drawing mode")
        }
    }

}