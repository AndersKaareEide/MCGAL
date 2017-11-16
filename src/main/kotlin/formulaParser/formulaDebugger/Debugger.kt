package formulaParser.formulaDebugger

import canvas.data.Model
import canvas.data.State
import formulaParser.Formula
import formulaParser.Knows
import formulaParser.buildSubformulaList
import formulaParser.formulaDebugger.Debugger.getAbsoluteIntRange
import formulaParser.getIndishStates
import formulafield.FormulaLabel
import javafx.collections.FXCollections
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

        stateLabelMap = convertToDebugLabels(labelItems, state)
        sortLabelMap(stateLabelMap)

        //Run checking to populate through calls to makeNextEntry()
        formula.check(state, model, this)

        assignStateLabels()

        return entryList
    }

    private fun sortLabelMap(stateLabelMap: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>>){
        stateLabelMap.keys.forEach {
            stateLabelMap[it]!!.sortByDescending { it.size }
        }
    }

    //Creates a mapping of all the subformulas contained in the input formula and state to
    private fun initValuationMap(formula: Formula, state: State): Map<Pair<State, Formula>,FormulaValue>{
        return buildSubformulaList(state, formula)
                .associateBy({ it }, { FormulaValue.UNKNOWN })
    }

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

    //TODO 1. Avoid recursion
    //TODO 2. Avoid fucking with indexes
    fun convertToDebugLabels(input: List<FormulaLabelItem>, state: State): MutableMap<State, MutableList<ObservableList<DebugLabelItem>>> {
        val result = mutableMapOf<State, MutableList<ObservableList<DebugLabelItem>>>()
        val debugLabelList = FXCollections.observableArrayList<DebugLabelItem>()
        //TODO Maybe make DebugLabelItems keep a reference to the FormulaLabelItem they are based on for linking purposes later on
        debugLabelList.addAll(input.map { DebugLabelItem(it.formula, it.labelText, it.indexRange, state) })

        distributeKnowsFormulas(debugLabelList, state)
        return result
    }

    //TODO Possibly deal with parentheses later

    fun distributeKnowsFormulas(debugLabelList: ObservableList<DebugLabelItem>, originState: State) {
        val knowsOpList = debugLabelList.filter { it.formula is Knows && !(it.labelText == "(" || it.labelText == ")") }
        val distributionMap = knowsOpList.associate { Pair(it, mutableSetOf<State>()) }

        //TODO If contained within previous Knows operator by checking index, distribute further
        for ((index, knowsOp) in knowsOpList.withIndex()){
            //For every step right, check all forms to the left to try and find a 'parent'
            for (innerIndex in (index - 1) downTo 0) {
                val parentOp = knowsOpList[innerIndex]
                if (parentOp.contains(debugLabelList, knowsOp)){
                    val parentOpDistributionSet = distributionMap[parentOp]
                    for (innerState in parentOpDistributionSet!!){
                        val formula = knowsOp.formula as Knows
                        distributionMap[knowsOp]!!.addAll(getIndishStates(formula.agent, innerState))
                    }
                    //We found a 'parent' for this one, continue to next formula
                    break
                }
            }
            val formula = knowsOp.formula as Knows
            distributionMap[knowsOp]!!.addAll(getIndishStates(formula.agent, originState))
        }

        originState.debugLabels.add(FXCollections.observableArrayList(debugLabelList))
        for (knowsOp in distributionMap.keys){
            val labels = getInnerLabels(knowsOp, debugLabelList)
            stripOuterParentheses(labels)

            //Actually add the labels to each state
            for (state in distributionMap[knowsOp]!!){
                state.debugLabels.add(FXCollections.observableArrayList(labels))
            }
        }
        //TODO 1. Find indishstates from all parentOp states
        //TODO 2. Add these to distributionMap for knowsOp
        //TODO 3. Add all indishstates from inputState as well
        //TODO 4. Somehow map from knowsOp to the DLI that represents its inner formula
        //TODO 5. Actually add the relevant lists of labels to the states
    }

    private fun stripOuterParentheses(labels: ObservableList<DebugLabelItem>) {
        if (labels[0].labelText == "("){
            labels.removeAt(0)
            labels.removeAt(labels.lastIndex)
        }
    }

    private fun getInnerLabels(knowsOp: DebugLabelItem, debugLabelList: ObservableList<DebugLabelItem>): ObservableList<DebugLabelItem>{
        val innerFormula = (knowsOp.formula as Knows).inner
        val innerLabel = debugLabelList.find { it.formula == innerFormula }!!

        val absRange = getAbsoluteIntRange(debugLabelList, innerLabel)

        return FXCollections.observableArrayList(debugLabelList.slice(absRange))
    }

    fun getAbsoluteIntRange(debugLabelList: ObservableList<DebugLabelItem>, innerLabel: DebugLabelItem): IntRange {
        val opIndex = debugLabelList.indexOf(innerLabel)
        val absRange = IntRange(innerLabel.indexRange.first + opIndex, innerLabel.indexRange.second + opIndex)
        return absRange
    }
}

private fun DebugLabelItem.contains(debugLabelList: ObservableList<DebugLabelItem>, other: DebugLabelItem): Boolean {
    val indexRange = getAbsoluteIntRange(debugLabelList, this)
    val otherRange = getAbsoluteIntRange(debugLabelList, other)

    return (indexRange.first < otherRange.first && indexRange.last >= otherRange.last)
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
