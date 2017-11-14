package formulaParser.formulaDebugger

import canvas.data.Model
import canvas.data.State
import formulaParser.Formula
import formulaParser.buildSubformulaList
import formulafield.FormulaLabel
import javafx.collections.ObservableList
import javafx.scene.control.IndexRange
import javafx.scene.layout.HBox
import sidepanels.debugpanel.DebugLabelItem
import sidepanels.debugpanel.FormulaLabelItem


//TODO Find out if this belongs in the DebugPanelController
object Debugger {

    lateinit var entryList: MutableList<DebugEntry>
    //TODO Unfuck, use list instead of Map so that shit doesn't get overwritten and cause negative index searches and fun stuff
    lateinit var labelItems: List<FormulaLabelItem>
    lateinit var valuationMap: Map<Pair<State, Formula>,FormulaValue>

    lateinit var formulaRangeMap: MutableMap<Pair<Formula, State>, IndexRange> //TODO (Formula, State) -> List?
    lateinit var stateLabelMap: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>>


    fun startDebug(formula: Formula, state: State, model: Model): MutableList<DebugEntry> {
        entryList = arrayListOf<DebugEntry>()
        valuationMap = initValuationMap(formula, state)
        labelItems = formula.toLabelItems()

        stateLabelMap = formula.toDebugLabelItems(state)
        sortLabelMap(stateLabelMap)
        filterLabelMap(stateLabelMap)

        //Run checking to populate through calls to makeNextEntry()
        formula.check(state, model, this)

//        stateLabelMap = filterStateLabelMap(stateLabelMap, entryList.last())
        assignStateLabels()

        return entryList
    }

    private fun sortLabelMap(stateLabelMap: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>>){
        stateLabelMap.keys.forEach {
            stateLabelMap[it]!!.sortByDescending { it.size }
        }
    }

    private fun filterLabelMap(stateLabelMap: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>>) {

    }

    //Creates a mapping of all the subformulas contained in the input formula and state to
    private fun initValuationMap(formula: Formula, state: State): Map<Pair<State, Formula>,FormulaValue>{
        return buildSubformulaList(state, formula)
                .associateBy({ it }, { FormulaValue.UNKNOWN })
    }

    //TODO Divide into <State, List<List<???>>>
    //TODO Somehow filter out top-level formulas that never get checked (never change formValue)
//    private fun filterStateLabelMap(stateLabelMap: MutableMap<State, MutableList<MutableList<DebugLabelItem>>>, lastEntry: DebugEntry): MutableMap<State, MutableList<MutableList<DebugLabelItem>>> {
//        return stateLabelMap.filter {
//            val debugLabelItem = stateLabelMap[it.key]!!.first()
//            val valuationMap = lastEntry.formValues
//            valuationMap[Pair(debugLabelItem.state, debugLabelItem.formula)] != FormulaValue.UNKNOWN
//        }.toMutableMap()
//    }

    //TODO Do this *after* the filtering?
    private fun assignStateLabels() {
        stateLabelMap.keys.forEach {
            it.debugLabels.setAll(stateLabelMap[it]!!)
        }
    }

    //Creates the next "log" entry based on the valuationMapping from the last entry
    fun makeNextEntry(formula: Formula, state: State, value: FormulaValue) {
        val labels = formula.toFormulaItem().labelItems.map { FormulaLabel(it) }
        val entry = if (entryList.isEmpty()){
            DebugEntry(state, labels, value, valuationMap, formula.depth)
        } else {
            val updatedFormValuation = entryList[entryList.lastIndex].formValues + Pair(Pair(state, formula), value)
            DebugEntry(state, labels, value, updatedFormValuation, formula.depth)
        }
        entryList.add(entry)
    }

}

//TODO Add edge traversal as a validation step for visual purposes?

//TODO Lage neste tilstand basert på forrige tilstand og den nye oppdateringen
class DebugEntry(val state: State, val labels: List<FormulaLabel>, val value: FormulaValue,
                 val formValues: Map<Pair<State,Formula>, FormulaValue>, val depth: Int){

    val labelbox: HBox = HBox()
    val stateNameProp = state.nameProperty

    init {
        labelbox.children.addAll(labels)
    }
}
