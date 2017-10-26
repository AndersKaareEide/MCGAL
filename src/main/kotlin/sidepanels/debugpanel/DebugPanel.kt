package sidepanels.debugpanel

import canvas.data.State
import formulaParser.Formula
import formulaParser.formulaDebugger.DebugEntry
import formulaParser.formulaDebugger.FormulaValue
import tornadofx.*

class DebugPanel : View("My View") {
    override val root = vbox {
        button("start") {
            //TODO Do whatever
        }
        listview {  }
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