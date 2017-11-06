package sidepanels.debugpanel

import canvas.controllers.CanvasController
import canvas.controllers.StateController
import formulaParser.FormulaParser
import formulaParser.formulaDebugger.DebugEntry
import formulaParser.formulaDebugger.Debugger
import formulafield.FormulaFieldController
import javafx.beans.property.SimpleListProperty
import tornadofx.*

class DebugController: Controller(){

    val canvasController = find(CanvasController::class)
    val stateController = find(StateController::class)
    val formulaController = find(FormulaFieldController::class)

    var debugEntries =  SimpleListProperty<DebugEntry>(mutableListOf<DebugEntry>().observable())
    var formulaText: String? = null

    /**
     * Starts sequence of choosing state to debug in and sets up debugger to generate list
     * of DebugEntries
     */
    fun startDebug(formulaText: String){
        this.formulaText = formulaText
        canvasController.stateSelectionCallback = this::runDebugger
        formulaController.errorMsgProperty.value = "Please select a state to step through the formula in"
    }

    private fun runDebugger() {
        canvasController.stateSelectionCallback = null
        formulaController.errorMsgProperty.value = ""

        val selectedState = stateController.selectedStates.first()
        val formula = FormulaParser.parse(formulaText!!, formulaController.errorMsgProperty)
        val model = canvasController.model

        debugEntries.setAll(Debugger.startDebug(formula, selectedState, model))
        canvasController.selectSidePanelTab(2)
    }
}