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

    val debugEntries =  SimpleListProperty<DebugEntry>(mutableListOf<DebugEntry>().observable())
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
        formulaController.setErrorMsg("Please select a state to step through the formula in")
    }

    private fun runDebugger(selectedState: State) {
        canvasController.stateSelectionCallback = null
        formulaController.clearErrorMsg()

        try {
            val formula = FormulaParser.parse(formulaText!!, formulaController::setErrorMsg)
            val model = canvasController.model

            debugEntries.setAll(Debugger.startDebug(formula, selectedState, model))

            canvasController.showDebugPanelTab()

            tableSelection.selectionModel.select(0)
            selectedEntryProperty.bind(tableSelection.selectionModel.selectedItemProperty())

        } catch (e: RecognitionException){
            formulaController.setErrorMsg(e.message!!)
        } catch (e: FormulaParsingException){
            formulaController.setErrorMsg(e.message!!)
        } catch (e: IllegalStateException){
            formulaController.setErrorMsg("Error parsing agents in group announcement")
        }
    }

    /**
     * Function responsible for coloring the DebugLabels next to each State by matching the formula
     * they represent with their corresponding value from the given DebugEntry's valuationMap
     */
    fun applyValuationMap(debugEntry: DebugEntry){
        formulaController.clearLabels()
        canvasController.clearSelectedComponents()
        canvasController.selectItem(debugEntry.state)
        val states = canvasController.model.states

        for (state in states){
            for (list in state.debugLabels){
                for (labelItem in list) {
                    labelItem.value = debugEntry.valuationMap[labelItem] ?: FormulaValue.UNKNOWN
                }
            }
            state.isHidden = !debugEntry.activeStates.contains(state)
        }
    }

    /**
     * Function used to clear any references to old stuff when for example loading a new model from file
     */
    fun clearDebugger(){
        canvasController.hideDebugPanelTab()
        debugEntries.clear()
        Debugger.clear()
        clearDebugLabels()
    }


    fun clearDebugLabels(){
        stateController.states.forEach {
            it.debugLabels.clear()
        }
    }

    fun stepInto() {
        tableSelection.requestFocus()
        tableSelection.selectionModel.selectNext()
    }

    fun stepOver() {
        tableSelection.requestFocus()
        val selectedIndex = tableSelection.selectionModel.selectedIndex
        val currentDepth = tableSelection.items[selectedIndex].depth

        for (index in selectedIndex + 1 until tableSelection.items.size){
            if (tableSelection.items[index].depth <= currentDepth) {
                tableSelection.selectionModel.select(index)
                break
            }
        }
    }
}