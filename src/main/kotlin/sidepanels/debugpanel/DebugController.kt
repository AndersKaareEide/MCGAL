package sidepanels.debugpanel

import canvas.controllers.CanvasController
import canvas.controllers.StateController
import canvas.data.State
import formulaParser.FormulaParser
import formulaParser.FormulaParsingException
import formulaParser.formulaDebugger.DebugEntry
import formulaParser.formulaDebugger.Debugger
import formulaParser.formulaDebugger.FormulaValue
import formulafield.FormulaFieldController
import javafx.beans.property.SimpleListProperty
import javafx.beans.property.SimpleObjectProperty
import javafx.scene.control.TableView
import org.antlr.v4.runtime.RecognitionException
import tornadofx.*

class DebugController: Controller(){

    val canvasController = find(CanvasController::class)
    val stateController = find(StateController::class)
    val formulaController = find(FormulaFieldController::class)

    var debugEntries =  SimpleListProperty<DebugEntry>(mutableListOf<DebugEntry>().observable())
    var formulaText: String? = null

    lateinit var tableSelection: TableView<DebugEntry>

    var selectedEntryProperty = SimpleObjectProperty<DebugEntry>()

    /**
     * Starts sequence of choosing state to debug in and sets up debugger to generate list
     * of DebugEntries
     */
    fun startDebug(formulaText: String){
        clearDebugger() //Clear any previous debugLabels

        this.formulaText = formulaText
        canvasController.stateSelectionCallback = this::runDebugger
        formulaController.errorMsgProperty.value = "Please select a state to step through the formula in"
    }

    private fun runDebugger() {
        canvasController.stateSelectionCallback = null
        formulaController.errorMsgProperty.value = ""

        val selectedState = stateController.selectedStates.first()

        try {
            val formula = FormulaParser.parse(formulaText!!, formulaController.errorMsgProperty)
            val model = canvasController.model

            debugEntries.setAll(Debugger.startDebug(formula, selectedState, model))
            canvasController.showDebugPanelTab()

            tableSelection.selectionModel.select(0)
            selectedEntryProperty.bind(tableSelection.selectionModel.selectedItemProperty())

        } catch (e: RecognitionException){
            formulaController.errorMsgProperty.value = e.message
        } catch (e: FormulaParsingException){
            formulaController.errorMsgProperty.value = e.message
        } catch (e: IllegalStateException){
            formulaController.errorMsgProperty.value = "Error parsing agents in group announcement"
        }
    }

    fun clearDebugger(){
        canvasController.hideDebugPanelTab()
        debugEntries.clear()
        clearDebugLabels()
    }

    //TODO Call when then user hits Esc or something
    fun clearDebugLabels(){
        stateController.states.forEach {
            it.debugLabels.clear()
        }
    }


    fun stepInto() {
        tableSelection.selectionModel.selectNext()
    }

    fun stepOver() {
        val selectedIndex = tableSelection.selectionModel.selectedIndex
        val currentDepth = tableSelection.items[selectedIndex].depth

        for (index in selectedIndex + 1 until tableSelection.items.size){
            if (tableSelection.items[index].depth <= currentDepth) {
                tableSelection.selectionModel.select(index)
                break
            }
        }
    }

    /**
     * Function responsible for coloring the DebugLabels next to each State by matching the formula
     * they represent with their corresponding value from the given DebugEntry's valuationMap
     */
    fun applyValuationMap(debugEntry: DebugEntry){
        canvasController.clearSelectedComponents()
        canvasController.selectItem(debugEntry.state)

        val hiddenStates = mutableMapOf<State, Boolean>()
        for (state in Debugger.stateLabelMap.keys){
            for (list in Debugger.stateLabelMap[state]!!){
                for (labelItem in list) {
                    val formulaValue = debugEntry.formValues[Pair(state, labelItem.formula)]!!
                    labelItem.value = formulaValue
                    if(labelItem.isAnnouncementCheck && formulaValue == FormulaValue.FALSE){
                        hiddenStates.put(state, true)
                    }
                }
            }
        }
        assignHiddenStates(Debugger.stateLabelMap.keys, hiddenStates)
    }

    private fun assignHiddenStates(states: MutableSet<State>, statesToHide: MutableMap<State, Boolean>) {
        for (state in states){
            state.isHidden = statesToHide[state] ?: false
        }
    }
}