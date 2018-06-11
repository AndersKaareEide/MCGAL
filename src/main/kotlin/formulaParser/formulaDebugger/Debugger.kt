package formulaParser.formulaDebugger

import canvas.data.Model
import canvas.data.State
import formulaParser.Formula
import formulafield.FormulaLabel
import javafx.collections.FXCollections
import javafx.collections.ObservableList
import sidepanels.debugpanel.DebugLabelItem
import sidepanels.debugpanel.FormulaLabelItem


//TODO Find out if this belongs in the DebugPanelController
object Debugger {

    //State, listDepth, formula
    private val formulaToLabelMap: MutableMap<Triple<State, Int, Formula>, DebugLabelItem> = mutableMapOf()
    private val stateToLabelRowsMap: MutableMap<State, MutableList<ObservableList<DebugLabelItem>>> = mutableMapOf()
    private var debugEntries: MutableList<DebugEntry> = mutableListOf()

    fun startDebug(formula: Formula, state: State, model: Model): List<DebugEntry> {
        stateToLabelRowsMap.putAll(model.states.associate { it to mutableListOf<ObservableList<DebugLabelItem>>() })

        addNewLabelRow(state, formula)

        //Run checking to populate through calls to makeNextEntry()
        formula.check(state, model, 0, this)

        return debugEntries
    }

    /**
     * Adds a new row of DebugLabels to the input state based on the formula as well as creating mappings between them
     * @return the index of the newly created row
     */
    fun addNewLabelRow(state: State, formula: Formula) : Int {
        val formLabels = formula.toLabelItems()
        val labelRow = createLabelRow(formLabels, state)
        val rowIndex = getNextRowIndex(state)

        stateToLabelRowsMap[state]!!.add(rowIndex, labelRow)
        state.debugLabelsProperty.add(rowIndex, labelRow)

        formulaToLabelMap.putAll(createLabelMappings(labelRow, rowIndex))
        return rowIndex
    }

    /**
     * Creates mappings from each label in a row's formula, state and index of the row it belongs to, back to the label
     */
    private fun createLabelMappings(labels: List<DebugLabelItem>, listIndex: Int): Map<out Triple<State, Int, Formula>, DebugLabelItem> =
            labels.associate { Triple(it.state, listIndex, it.formula) to it }

    /**
     * Creates a new row of DebugLabels based on a formula, represented as a list of FormulaLabelItems
     */
    private fun createLabelRow(input: List<FormulaLabelItem>, state: State): ObservableList<DebugLabelItem> {
        val debugLabelRow = FXCollections.observableArrayList<DebugLabelItem>()
        debugLabelRow.addAll(input.map { DebugLabelItem(it.formula, it.labelText, it.indexRange, state) })

        return debugLabelRow
    }

    /**
     * Returns the next available index for use when distributing DebugLabels for Knows or Announcement formulas
     */
    private fun getNextRowIndex(state: State): Int = stateToLabelRowsMap[state]?.size ?: 0

    //Creates the next "log" entry based on the valuationMapping from the last entry
    fun makeNextEntry(formula: Formula, state: State, listDepth: Int, value: FormulaValue, activeStates: List<State>) {
        val labels = formula.toFormulaItem().labelItems.map { FormulaLabel(it) }
        val debugLabelItem = formulaToLabelMap[Triple(state, listDepth, formula)]!!

        val updatedFormValuation = if (debugEntries.isEmpty()){
            mapOf(debugLabelItem to value)
        } else {
            debugEntries.last().valuationMap + (debugLabelItem to value)
        }

        val entry = DebugEntry(state, labels, value, updatedFormValuation, formula.depth, activeStates)
        debugEntries.add(entry)
    }

    fun clear(){
        debugEntries.clear()
        formulaToLabelMap.clear()
        stateToLabelRowsMap.clear()
    }
}
