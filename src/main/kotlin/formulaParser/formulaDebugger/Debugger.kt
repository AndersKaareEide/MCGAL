package formulaParser.formulaDebugger

import canvas.data.Model
import canvas.data.State
import formulaParser.Formula
import formulaParser.buildSubformulaList
import formulafield.FormulaLabel
import javafx.scene.Parent
import sidepanels.debugpanel.FormulaLabelItem
import tornadofx.*


//TODO Find out if this belongs in the DebugPanelController
class Debugger {

    val entryList = arrayListOf<DebugEntry>()
    lateinit var labelItems: Map<Formula, FormulaLabel>
    lateinit var valuationMap: Map<Pair<State, Formula>,FormulaValue>

    fun startDebug(formula: Formula, state: State, model: Model){
        valuationMap = initValuationMap(formula, state)
        labelItems = formula.toLabelItems()
                .map { FormulaLabel(it) }
                .associateBy({ it.formula },{ it })

        //Run checking to populate through calls to makeNextEntry()
        formula.check(state, model, this)

        println(entryList)
    }

    //TODO Formalize relation between formula and state
    //Creates a mapping of all the subformulas contained in the input formula and state to
    fun initValuationMap(formula: Formula, state: State): Map<Pair<State, Formula>,FormulaValue>{
        return buildSubformulaList(formula)
                .map { it -> Pair(state, it) }
                .associateBy({ it }, { FormulaValue.UNKNOWN })
    }

    //TODO Somehow link the KA(p) subformulas to the correct states
    //Creates the next "log" entry based on the valuationMapping from the last entry
    fun makeNextEntry(formula: Formula, state: State, value: FormulaValue) {
        val entry = if (entryList.isEmpty()){
            DebugEntry(state, labelItems[formula]!!, value, valuationMap)
        } else {
            val updatedFormValuation = entryList[entryList.lastIndex].formValues + Pair(Pair(state, formula), value)
            DebugEntry(state, labelItems[formula]!!, value, updatedFormValuation)
        }
        entryList.add(entry)
    }

}

//TODO Add edge traversal as a validation step for visual purposes?

//TODO Use formula depth to step over subformulas during debugging

//TODO Lage neste tilstand basert på forrige tilstand og den nye oppdateringen
class DebugEntry(val state: State, val formulaLabel: FormulaLabel, val value: FormulaValue,
                 val formValues: Map<Pair<State,Formula>, FormulaValue>)
