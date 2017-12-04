package sidepanels.debugpanel

import canvas.controllers.CanvasController
import canvas.data.AgentItem
import formulaParser.Knows
import formulaParser.Proposition
import formulaParser.formulaDebugger.DebugEntry
import javafx.scene.control.SelectionMode
import javafx.scene.layout.Priority
import sidepanels.propertypanel.PropositionItem
import tornadofx.*

class DebugPanel : View("My View") {

    val controller = find(CanvasController::class)
    val debugController = find(DebugController::class)

    val form = Knows(AgentItem("Arne", false),
            Proposition(PropositionItem("regn", false), 1), 0)

    val model = controller.model

    override val root = vbox {
        hbox {
            button("Over") {
                action { debugController.stepOver() }
            }
            button("Into") {
                action { debugController.stepInto() }
            }
        }

        debugController.tableSelection = tableview(debugController.debugEntries) {
            selectionModel.selectionMode = SelectionMode.SINGLE

            setSortPolicy { false }
            column("", DebugEntry::stateNameProp).contentWidth()
            column("Formula", DebugEntry::labelbox).pctWidth(80)
            column("Val", DebugEntry::value).contentWidth()

            vgrow = Priority.ALWAYS
            smartResize()

            selectionModel.selectedIndexProperty().onChange {
                if (it >= 0)
                    debugController.applyValuationMap(items[it])
            }
        }

    }
}