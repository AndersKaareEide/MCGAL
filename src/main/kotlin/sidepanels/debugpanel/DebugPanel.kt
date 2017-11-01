package sidepanels.debugpanel

import canvas.controllers.CanvasController
import canvas.data.AgentItem
import formulaParser.*
import formulaParser.formulaDebugger.DebugEntry
import formulaParser.formulaDebugger.Debugger
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
        button("start") {
            action { debugController.debugEntries.setAll(Debugger.startDebug(form, model.states.first(), model).observable()) }
        }

        tableview(debugController.debugEntries) {
            //TODO Make it look nicer before table is filled
            setSortPolicy { false }
            column("", DebugEntry::stateNameProp).contentWidth()
            column("Formula", DebugEntry::labelbox).pctWidth(80)
            column("Val", DebugEntry::value).contentWidth()

            vgrow = Priority.ALWAYS
            smartResize()
        }

    }
}

class DebugEntryListFragment(): ListCellFragment<DebugEntry>(){
    override val root = hbox {
        text(item.state.name)
    }
    //TODO onSelectionChanged -> Update all LabelItems based on the valuationMap from the DebugEntry
    //TODO Make LabelItem -> Pair<DebugLabel, FormulaLabel> mapping
}


//Add breakpoints as subclass of formula ]}
class FormViewItem()