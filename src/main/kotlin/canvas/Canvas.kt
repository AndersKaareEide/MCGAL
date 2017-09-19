package canvas

import javafx.collections.FXCollections
import javafx.scene.input.KeyCombination
import tornadofx.*

class Canvas : View("My View") {

    val controller: CanvasController by inject()

    override val root = borderpane {
        prefWidth = 800.0
        prefHeight = 600.0

        right = AgentPanel().root

        center = stackpane {

            setOnMouseClicked { controller.handleCanvasClick(it) }

            anchorpane {
                isManaged = false
                bindChildren(controller.edges) {
                    EdgeFragment(it).root
                }
            }
            anchorpane {
                isManaged = false
                bindChildren(controller.states) {
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
