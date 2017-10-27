package sidepanels.debugpanel

import canvas.controllers.CanvasController
import canvas.data.AgentItem
import canvas.data.State
import formulaParser.*
import formulaParser.formulaDebugger.DebugEntry
import formulaParser.formulaDebugger.Debugger
import formulaParser.formulaDebugger.FormulaValue
import formulafield.FormulaFieldController
import javafx.beans.property.SimpleListProperty
import sidepanels.propertypanel.PropositionItem
import tornadofx.*

class DebugPanel : View("My View") {

    val controller = find(CanvasController::class)
    var entries =  SimpleListProperty<DebugEntry>(mutableListOf<DebugEntry>().observable())

    val form = Knows(AgentItem("Arne", false),
            Proposition(PropositionItem("regn", false), 1), 0)

    val model = controller.model

    override val root = vbox {
        button("start") {
            action { entries.setAll(Debugger().startDebug(form, model.states.first(), model).observable()) }
        }

        tableview(entries) {
            setSortPolicy { false }
            column("", DebugEntry::stateNameProp).contentWidth()
            column("Formula", DebugEntry::labelbox).pctWidth(80)
            column("Val", DebugEntry::value).contentWidth()

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