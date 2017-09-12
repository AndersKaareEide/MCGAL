package canvas

import javafx.collections.FXCollections
import javafx.scene.input.KeyCombination
import tornadofx.*

class Canvas : View("My View") {

    val controller: CanvasController by inject()
    val state1 = State("s1", 150.0, 200.0)
    val state2 = State("s2", 50.0, 70.0, "p, q")

    //TODO Remove, used for manual testing purposes only
    val states = FXCollections.observableArrayList(state1, state2)
    val edges = FXCollections.observableArrayList(Edge(state1, state2, listOf(AgentItem("a", true))))

    override val root = borderpane {
        prefWidth = 800.0
        prefHeight = 600.0

        right = AgentPanel().root

        center = stackpane {

            setOnMouseClicked { controller.handleCanvasClick(it) }

            anchorpane {
                bindChildren(edges) {
                    EdgeFragment(it).root
                }
            }
            anchorpane {
                bindChildren(states) {
                    StateFragment(it).root
                }
            }
        }

        bottom = hbox {
            //TODO Prevent from being squeezed out of view
            checkbox {
                controller.isDrawingLinesProperty.bind(selectedProperty())
                text = "_Line drawing mode"
                accelerators.put(KeyCombination.keyCombination("ALT+L")) { fire() }
            }
        }
    }
}
