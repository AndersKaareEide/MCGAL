package canvas

import javafx.scene.input.KeyCode
import javafx.scene.input.KeyCombination
import javafx.scene.input.KeyEvent
import tornadofx.*

class Canvas : View("My View") {

    val controller: CanvasController by inject()
    val formulaController: FormulaFieldController by inject()

    //TODO Move out into own 'view'

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
                //TODO Figure out how to display which states satisfy the formula
                //Use visibleWhen on both States and Edges? Edges visible only when both of its attached states are
                bindChildren(controller.states) {
                    StateFragment(it).root
                }
            }
        }
        bottom = vbox {
            //Label used to display error messages
            label(formulaController.errorMsgProperty) {
                removeWhen {
                    textProperty().isEmpty
                }
            }
            hbox {
                checkbox(property = controller.isDrawingLinesProperty) {
                    text = "_Line drawing mode"
                    accelerators.put(KeyCombination.keyCombination("ALT+L")) { fire() }
                }
                //TODO Make into its own component
                textfield {
                    //TODO Clear error field when user resumes editing formula
                    promptText = "Write formulas here"
                    setOnAction { formulaController.validateFormString(text, controller.model) }
                    accelerators.put(KeyCombination.keyCombination("Esc")) {
                        formulaController.clearValidation(controller.model)
                    }
//                    textProperty().onChange { formulaController.clearValidation(controller.model) }
                }
            }

        }
    }
}
