package sidepanels.debugpanel

import canvas.data.State
import formulaParser.Formula
import formulaParser.formulaDebugger.ExecutionStep
import formulaParser.formulaDebugger.FormulaValue
import tornadofx.*

class DebugPanel : View("My View") {
    override val root = borderpane {

    }
}

class DebugStepListFragment(step: ExecutionStep, labelStates: Map<Pair<State, Formula>, FormulaValue>): {

}

