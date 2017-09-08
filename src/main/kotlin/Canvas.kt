import javafx.beans.property.BooleanProperty
import javafx.beans.property.SimpleObjectProperty
import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene.layout.*
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
import javafx.scene.text.TextAlignment
import tornadofx.*
import javax.naming.ldap.Control

class Canvas : View("My View") {

    val controller: CanvasStateController by inject()
    val states = FXCollections.observableArrayList(
            State("s1", 0.0, 0.0),
            State("s2", 50.0, 70.0, "p, q"))

    override val root = borderpane {
        prefHeight = 600.0
        prefWidth = 600.0

        center = anchorpane {
            bindChildren(states) {
                StateView(it).root
            }
            setOnMouseDragReleased { controller.handleDragEnd(null) }
        }

        bottom = hbox {
            checkbox {
                controller.isDrawingLinesProperty.bind(selectedProperty())
            }
            label("Line drawing mode")
        }
    }

}