package sidepanels.debugpanel

import canvas.controllers.CanvasController
import canvas.data.State
import formulaParser.Conjunction
import formulaParser.Formula
import formulaParser.Proposition
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

    val form = Conjunction(
            Conjunction(
                    Proposition(PropositionItem("p", false), 2),
                    Proposition(PropositionItem("q", false), 2), 1),
            Proposition(PropositionItem("r", false), 1), 1)

    val model = controller.model

    override val root = vbox {
        button("start") {
            action { entries.setAll(Debugger().startDebug(form, model.states.first(), model).observable()) }
        }

        tableview(entries) {
            setSortPolicy { false }
            column("", DebugEntry::stateNameProp)
            column("Formula", DebugEntry::labelbox)
            column("Val", DebugEntry::value)
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