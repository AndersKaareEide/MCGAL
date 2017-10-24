package formulaParser.formulaDebugger

import canvas.data.State
import canvas.data.Model
import formulaParser.Formula
import formulaParser.buildSubformulaList
import formulafield.FormulaLabel
import javafx.scene.Parent
import tornadofx.*
import java.util.*

val entryList = arrayListOf<DebugEntry>()
val valuationMap = mutableMapOf<Pair<State, Formula>,FormulaValue>()

//TODO Find out if this belongs in the DebugPanelController
class Debugger {
    fun startDebug(formula: Formula, state: State, model: Model){
        formula.check(state, model, this)

        //TODO Kanskje skrote hele ExecutionStep? Druse gjennom hele driten rekursivt som vanlig, men lage logg som brukeren kan steppe gjennom
    }

    //TODO Formalize relation between formula and state
    //Creates a mapping of all the subformulas contained in the input formula and state to
    fun initValuationMap(formula: Formula, state: State): Map<Pair<State, Formula>,FormulaValue>{
        return buildSubformulaList(formula)
                .map { it -> Pair(state, it) }
                .associateBy({ it }, { FormulaValue.UNKNOWN })
    }

    //Creates the next "log" entry based on the last entry and
    fun makeNextEntry(formula: Formula, state: State, value: FormulaValue): DebugEntry {
        val updatedFormValuation = entryList[entryList.lastIndex].formValues + Pair(Pair(formula, state), value)
        return DebugEntry(state)
    }

}

//TODO Add edge traversal as a validation step for visual purposes?

//TODO Use formula depth to step over subformulas during debugging

//TODO Lage neste tilstand basert på forrige tilstand og den nye oppdateringen
class DebugEntry(val state: State, val formulaLabel: FormulaLabel, val value: FormulaValue,
                 val formValues: Map<Pair<State,Formula>, FormulaValue>)

class DebugEntryListFragment(): ListCellFragment<DebugEntry>(){
    override val root: Parent
        get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.

}


//Add breakpoints as subclass of formula
class FormViewItem()