package sidepanels.debugpanel

import canvas.data.State
import formulaParser.Formula
import formulaParser.formulaDebugger.DebugEntry
import formulaParser.formulaDebugger.FormulaValue
import formulafield.FormulaLabel

class DebugPanelController {

    val entryList = arrayListOf<DebugEntry>()
    lateinit var labelItems: Map<Formula, FormulaLabel>
    lateinit var valuationMap: Map<Pair<State, Formula>, FormulaValue>


}