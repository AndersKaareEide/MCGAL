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


    /**
     * Starts sequence of choosing state to debug in and sets up debugger to generate list
     * of DebugEntries
     */
    fun startDebug(text: String) {
        //TODO Make user pick state to debug in
        formulaController.errorMsgProperty.value = ""

        if (stateController.selectedStates.isNotEmpty()) {
            val selectedState = stateController.selectedStates.first()
            val formula = FormulaParser.parse(text, formulaController.errorMsgProperty)
            val model = canvasController.model

            debugEntries.setAll(Debugger.startDebug(formula, selectedState, model))
            canvasController.selectSidePanelTab(2)
        }
    }
}