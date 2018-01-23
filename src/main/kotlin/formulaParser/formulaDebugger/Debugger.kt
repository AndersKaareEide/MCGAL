package formulaParser.formulaDebugger

import canvas.controllers.CanvasController
import canvas.data.Model
import canvas.data.State
import formulaParser.*
import formulafield.FormulaLabel
import javafx.collections.FXCollections
import javafx.collections.ObservableList
import sidepanels.debugpanel.DebugLabelItem
import sidepanels.debugpanel.FormulaLabelItem
import tornadofx.*


//TODO Find out if this belongs in the DebugPanelController
object Debugger {

    private val canvasController = find(CanvasController::class)

    private lateinit var debugEntries: MutableList<DebugEntry>

    private lateinit var labelItems: List<FormulaLabelItem>
    private lateinit var valuationMap: Map<Pair<State, Formula>,FormulaValue>

    lateinit var stateLabelMap: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>>

    fun startDebug(formula: Formula, state: State, model: Model): MutableList<DebugEntry> {
        debugEntries = arrayListOf<DebugEntry>()
        valuationMap = initValuationMap(formula, state)
        labelItems = formula.toLabelItems()

        stateLabelMap = convertToDebugLabels(labelItems, state)
        sortLabelMap(stateLabelMap)

        //Run checking to populate through calls to makeNextEntry()
        formula.check(state, model, this)
        return debugEntries
    }

    private fun sortLabelMap(stateLabelMap: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>>){
        stateLabelMap.keys.forEach {
            stateLabelMap[it]!!.sortByDescending { it.size }
        }
    }

    //Creates a mapping of all the subformulas contained in the input formula and state to
    private fun initValuationMap(formula: Formula, state: State): Map<Pair<State, Formula>,FormulaValue>{
        return buildSubformulaList(state, formula, canvasController.model )
                .associateBy({ it }, { FormulaValue.UNKNOWN })
    }

    //Creates the next "log" entry based on the valuationMapping from the last entry
    fun makeNextEntry(formula: Formula, state: State, value: FormulaValue) {
        val labels = formula.toFormulaItem().labelItems.map { FormulaLabel(it) }
        val updatedFormValuation = if (debugEntries.isEmpty()){
            valuationMap + Pair(Pair(state, formula), value)
        } else {
            debugEntries[debugEntries.lastIndex].formValues + Pair(Pair(state, formula), value)
        }

        val entry = DebugEntry(state, labels, value, updatedFormValuation, formula.depth)
        debugEntries.add(entry)
    }

    private fun convertToDebugLabels(input: List<FormulaLabelItem>, originState: State): MutableMap<State, MutableList<ObservableList<DebugLabelItem>>> {

        val result = canvasController.model.states
                .associate { Pair(it, mutableListOf<ObservableList<DebugLabelItem>>()) }
                .toMutableMap()

        val debugLabelList = FXCollections.observableArrayList<DebugLabelItem>()
        debugLabelList.addAll(input.map { DebugLabelItem(it.formula, it.labelText, it.indexRange, originState) })

        val originalLabels = FXCollections.observableArrayList(debugLabelList)
        originState.debugLabels.add(originalLabels)
        result[originState]!!.add(originalLabels)

        distributeKnowsFormulas(debugLabelList, originState, result)
        distributeAnnouncements(debugLabelList, result)
        return result
    }

    private fun distributeKnowsFormulas(debugLabelList: ObservableList<DebugLabelItem>, originState: State,
                                        result: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>>) {

        val knowsOpList = debugLabelList.filter { it.formula is Knows && !(it.labelText == "(" || it.labelText == ")") }
        val distributionMap = knowsOpList.associate { Pair(it, mutableSetOf<State>()) }
        val model = canvasController.model //We don't actually care about the model here, as we're not checking

        for ((index, knowsOp) in knowsOpList.withIndex()){
            //For every step right, check all forms to the left to try and find a 'parent'
            for (innerIndex in (index - 1) downTo 0) {
                val parentOp = knowsOpList[innerIndex]
                if (parentOp.contains(debugLabelList, knowsOp)){
                    val parentOpDistributionSet = distributionMap[parentOp]
                    for (innerState in parentOpDistributionSet!!){
                        val formula = knowsOp.formula as Knows
                        distributionMap[knowsOp]!!.addAll(getIndishStates(formula.agent, innerState, model))
                    }
                    //We found a 'parent' for this one, continue to next formula
                    break
                }
            }
            val formula = knowsOp.formula as Knows
            distributionMap[knowsOp]!!.addAll(getIndishStates(formula.agent, originState, model))
        }

        for (knowsOp in distributionMap.keys){
            val labels = getInnerLabels(knowsOp, debugLabelList)
            stripOuterParentheses(labels)

            //Actually add the labels to each state
            for (state in distributionMap[knowsOp]!!){
                val labelCopies = FXCollections.observableArrayList(labels.map {
                    DebugLabelItem(it.formula, it.labelText, it.indexRange, it.state)
                })
                state.debugLabels.add(labelCopies)
                result[state]!!.add(labelCopies)
            }
        }
        //1. Find indishstates from all parentOp states
        //2. Add these to distributionMap for knowsOp
        //3. Add all indishstates from inputState as well
        //4. Somehow map from knowsOp to the DLI that represents its inner formula
        //5. Actually add the relevant lists of labels to the states
    }

    /**
     * Function responsible for creating DebugLabels for any announcement formulas and
     * distributing them across the model
     */
    private fun distributeAnnouncements(debugLabelList: ObservableList<DebugLabelItem>, result: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>>) {

        val announcementLabelList = debugLabelList
                .filter { it.formula is Announcement }
                .filter { (it.labelText == "[") } //Announcements are weird since they are only represented as [ and ]

        val dix = announcementLabelList.map { getAnnouncementLabels(it, debugLabelList) }

        for (state in canvasController.model.states){
            val labelCopies = dix.map {
                FXCollections.observableArrayList(it.map {
                    DebugLabelItem(it.formula, it.labelText, it.indexRange, it.state, isAnnouncementCheck = true)
                })
            }.toMutableList()
            result[state]!!.addAll(labelCopies)
        }
    }

    private fun stripOuterParentheses(labels: ObservableList<DebugLabelItem>) {
        if (labels[0].labelText == "("){
            labels.removeAt(0)
            labels.removeAt(labels.lastIndex)
        }
    }

    fun getAbsoluteIntRange(debugLabelList: ObservableList<DebugLabelItem>, innerLabel: DebugLabelItem): IntRange {
        val opIndex = debugLabelList.indexOf(innerLabel)
        val absRange = IntRange(innerLabel.indexRange.first + opIndex, innerLabel.indexRange.last + opIndex)
        return absRange
    }

    fun clear(){
        debugEntries.clear()
        labelItems = emptyList()
        valuationMap = emptyMap()
        stateLabelMap = mutableMapOf()
    }
}



