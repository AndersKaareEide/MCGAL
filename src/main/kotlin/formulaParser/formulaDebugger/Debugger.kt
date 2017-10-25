package formulaParser.formulaDebugger

import canvas.data.State
import canvas.data.Model
import formulaParser.Formula
import formulaParser.buildSubformulaList
import formulafield.FormulaLabel
import javafx.scene.Parent
import sidepanels.debugpanel.DebugLabelItem
import tornadofx.*
import java.util.*

val entryList = arrayListOf<DebugEntry>()
val labelItems = arrayListOf<DebugLabelItem>()
val valuationMap = mutableMapOf<Pair<State, Formula>,FormulaValue>()

//TODO Find out if this belongs in the DebugPanelController
class Debugger {
    fun startDebug(formula: Formula, state: State, model: Model){
        valuationMap.putAll(initValuationMap(formula, state))
        labelItems.addAll(formula.toLabels().map { DebugLabelItem(it.formula, it.labelText, it.indexRange, state) })
        formula.check(state, model, this)

    }

    //TODO Formalize relation between formula and state
    //Creates a mapping of all the subformulas contained in the input formula and state to
    fun initValuationMap(formula: Formula, state: State): Map<Pair<State, Formula>,FormulaValue>{
        return buildSubformulaList(formula)
                .map { it -> Pair(state, it) }
                .associateBy({ it }, { FormulaValue.UNKNOWN })
    }

    //TODO Somehow link the KA(p) subformulas to the correct states
    //Creates the next "log" entry based on the last entry and
    fun makeNextEntry(formula: Formula, state: State, value: FormulaValue): DebugEntry {
        if (entryList.isEmpty()){
            return DebugEntry(state, DebugLabelItem(formula.toLabels()))
        }
        val updatedFormValuation = entryList[entryList.lastIndex].formValues + Pair(Pair(formula, state), value)
        return DebugEntry(state)
    }

}

//TODO Add edge traversal as a validation step for visual purposes?

//TODO Use formula depth to step over subformulas during debugging

//TODO Lage neste tilstand basert på forrige tilstand og den nye oppdateringen
class DebugEntry(val state: State, val labelItem: DebugLabelItem, val value: FormulaValue,
                 val formValues: Map<Pair<State,Formula>, FormulaValue>){

}

class DebugEntryListFragment(): ListCellFragment<DebugEntry>(){
    override val root: Parent
        get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.

    //TODO onSelectionChanged -> Update all LabelItems based on the valuationMap from the DebugEntry
    //TODO Make LabelItem -> Pair<DebugLabel, FormulaLabel> mapping
}


//Add breakpoints as subclass of formula
class FormViewItem()