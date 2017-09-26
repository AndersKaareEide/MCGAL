package canvas.views

import canvas.FormulaFieldController
import canvas.controllers.CanvasController
import canvas.controllers.EdgeController
import canvas.controllers.StateController
import javafx.scene.control.TabPane
import javafx.scene.input.KeyCombination
import menus.CanvasMenuBar
import sidepanels.agentpanel.AgentPanel
import sidepanels.propertypanel.PropositionPanel
import tornadofx.*

class Canvas : View("My View") {

    val controller: CanvasController by inject()
    val stateController: StateController by inject()
    val edgeController: EdgeController by inject()
    val formulaController: FormulaFieldController by inject()

    //TODO Move out into own 'view'

    override val root = borderpane {
        prefWidth = 800.0
        prefHeight = 600.0

        top = CanvasMenuBar.root

        right = tabpane() {
            prefWidth = 200.0

            tabClosingPolicy = TabPane.TabClosingPolicy.UNAVAILABLE

            tab("Agents", AgentPanel().root)
            tab("Propositions", PropositionPanel().root)
        }

        center = stackpane {

            setOnMouseClicked { stateController.handleCanvasClick(it) }

            anchorpane {
                isManaged = false
                bindChildren(edgeController.edges) {
                    EdgeFragment(it).root
                }
            }
            anchorpane {
                isManaged = false
                //TODO Figure out how to display which states satisfy the formula
                //Use visibleWhen on both States and Edges? Edges visible only when both of its attached states are
                bindChildren(stateController.states) {
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
                checkbox(property = stateController.isDrawingLinesProperty) {
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
                    //TODO Do I want to clear the validation when the user edits the formula? Live validation perhaps?
                    //Might be really slow for more complex formulas / models
//                    textProperty().onChange { formulaController.clearValidation(controller.model) }
                }
            }

        }
    }
}

