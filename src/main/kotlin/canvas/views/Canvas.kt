package canvas.views

import formulafield.FormulaFieldController
import canvas.controllers.CanvasController
import canvas.controllers.DragBoxController
import canvas.controllers.EdgeController
import canvas.controllers.StateController
import formulafield.FormulaField
import javafx.geometry.Orientation
import javafx.scene.control.TabPane
import javafx.scene.input.KeyCode
import javafx.scene.layout.Priority
import menus.CanvasMenuBar
import sidepanels.agentpanel.AgentPanel
import sidepanels.debugpanel.DebugPanel
import sidepanels.propertypanel.PropositionPanel
import tornadofx.*

class Canvas : View("My View") {

    private val controller: CanvasController by inject()
    private val dBoxController: DragBoxController by inject()
    private val stateController: StateController by inject()
    private val edgeController: EdgeController by inject()
    private val formulaController: FormulaFieldController by inject()

    val sidePanel = TabPane()
    //TODO Move out into own 'view'

    override val root = splitpane(Orientation.HORIZONTAL) {
        setDividerPositions(100.0) //Hack just to get the divider to give maximum space to the canvas

        borderpane {
            prefWidth = 800.0
            prefHeight = 600.0
            vgrow = Priority.ALWAYS
            hgrow = Priority.ALWAYS

            center = stackpane {
                anchorpane {
                    isManaged = false
                    isPickOnBounds = false
                    bindChildren(edgeController.edges) {
                        EdgeFragment(it).root
                    }
                }
                anchorpane {
                    isManaged = false
                    isPickOnBounds = false
                    bindChildren(stateController.states) {
                        StateFragment(it).root
                    }
                }
                anchorpane {
                    isManaged = false
                    isPickOnBounds = false
                    bindChildren(stateController.states) {
                        DebugLabelHolder(it).root
                    }
                }
                dragrectangle()
            }

            top = CanvasMenuBar.root

            bottom = FormulaField().root
        }

        //TODO Somehow re-route KeyEvents so that ctrl-tab still changes pane even when states are focused
        add(sidePanel)
        with(sidePanel) {

            prefWidth = 215.0
            minWidth = 215.0

            tabClosingPolicy = TabPane.TabClosingPolicy.UNAVAILABLE

            tab("Agents", AgentPanel().root)
            tab("Propositions", PropositionPanel().root)
        }

        //KeyEvent re-routing, here be dragons
        this@splitpane.setOnKeyPressed {
            if(it.isShortcutDown && it.code == KeyCode.TAB) {
                sidePanel.fireEvent(it) //Dirty solution to re-route Ctrl-Tab presses to the sidePanel
            } else if (it.code == KeyCode.DELETE) {
                controller.removeSelected()
            } else if (it.code == KeyCode.ESCAPE) {
                formulaController.clearValidation()
                formulaController.clearLabels()
            } else if (it.isAltDown && it.code == KeyCode.A){
                stateController.selectAll()
            } else if (it.isAltDown && it.code == KeyCode.C){
                controller.copySelection()
            } else if (it.isAltDown && it.code == KeyCode.V){
                controller.pasteComponents()
            }
        }
        //Drag selection
        setOnMousePressed { controller.handleCanvasClick(it); formulaController.clearValidation() }
        setOnDragDetected { dBoxController.handleCanvasDragStart(it) }
        setOnMouseDragged { dBoxController.handleCanvasDrag(it) }

        //TODO Fix OS 'banner' calling onExited somehow
        setOnMouseDragExited {
            stateController.recenterSelectedStates()
            dBoxController.handleCanvasDragEnd(it)
        }
    }
}


