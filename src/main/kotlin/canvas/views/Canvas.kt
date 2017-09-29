package canvas.views

import canvas.FormulaFieldController
import canvas.controllers.CanvasController
import canvas.controllers.DragBoxController
import canvas.controllers.EdgeController
import canvas.controllers.StateController
import javafx.scene.control.TabPane
import javafx.scene.input.KeyCode
import javafx.scene.input.KeyCombination
import menus.CanvasMenuBar
import sidepanels.agentpanel.AgentPanel
import sidepanels.propertypanel.PropositionPanel
import tornadofx.*

class Canvas : View("My View") {

    private val controller: CanvasController by inject()
    private val dBoxController: DragBoxController by inject()
    private val stateController: StateController by inject()
    private val edgeController: EdgeController by inject()
    private val formulaController: FormulaFieldController by inject()

    //TODO Move out into own 'view'

    override val root = hbox {

        borderpane {
            prefWidth = 800.0
            prefHeight = 600.0

            top = CanvasMenuBar.root

            center = stackpane {

                //Drag selection
                setOnMouseClicked { stateController.handleCanvasClick(it) }
                setOnDragDetected { dBoxController.handleCanvasDragStart(it) }
                setOnMouseDragged { dBoxController.handleCanvasDrag(it) }
                setOnMouseDragReleased { dBoxController.handleCanvasDragEnd(it) }
                //TODO Fix other components eating these events


                dragrectangle()

                anchorpane {
                    isManaged = false
                    bindChildren(edgeController.edges) {
                        EdgeFragment(it).root
                    }

                }
                anchorpane {
                    isManaged = false
                    bindChildren(stateController.states) {
                        StateFragment(it).root
                    }
                }
            }
            //TODO Make into its own component
            bottom = vbox {
                //Label used to display error messages
                label(formulaController.errorMsgProperty) {
                    removeWhen {
                        textProperty().isEmpty
                    }
                }
                textfield {
                    //TODO Clear error field when user resumes editing formula
                    promptText = "Write formulas here"
                    setOnAction { formulaController.validateFormString(text, controller.model) }
                    accelerators.put(KeyCombination.keyCombination("Esc")) {
                        formulaController.clearValidation(controller.model)
                    }
                    //TODO Do I want to clear the validation when the user edits the formula? Live validation perhaps?
                    //Might be really slow for more complex formulas / models
//                    textProperty().onChange { formulaController.clearValidation(edgeController.model) }
                }
            }
        }

        //TODO Somehow re-route KeyEvents so that ctrl-tab still changes pane even when states are focused
        val sidepanel = tabpane {
            prefWidth = 200.0

            tabClosingPolicy = TabPane.TabClosingPolicy.UNAVAILABLE

            tab("Agents", AgentPanel().root)
            tab("Propositions", PropositionPanel().root)
        }

        //KeyEvent re-routing, here be dragons
        this@hbox.setOnKeyPressed {
            if(it.isShortcutDown && it.code == KeyCode.TAB) {
                sidepanel.fireEvent(it) //Dirty solution to re-route Ctrl-Tab presses to the sidepanel
            } else if (it.code == KeyCode.DELETE) {
                stateController.removeSelected()
            }
        }

    }
}

