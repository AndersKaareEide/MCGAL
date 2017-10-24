package formulaParser.formulaDebugger

import canvas.data.Model
import canvas.data.State
import formulaParser.Formula
import formulaParser.Proposition
import sidepanels.propertypanel.PropositionItem

abstract class ExecutionStep {
}

//TODO Rename
class Executable(val formula: Formula, val numBooleans: Int, val state:State, val model: Model,
                 val op: (Boolean) -> Boolean) : ExecutionStep() {
    //TODO Find out how many booleans are needed to check the formula


    fun check(input: Boolean): ExecutionResult {
        
    }

    //TODO Add skip function to skip stepping through subformula

}

class AtomicValidationStep(val prop: PropositionItem, val state: State, val model: Model) : ExecutionStep(){
    fun check(): ExecutionResult {
        return ExecutionResult(state.props.contains(prop))
    }
}

//TODO It hurts to live
class ExecutionResult(input: Boolean) : ExecutionStep() {
    val result: FormulaValue = if (input){
        FormulaValue.TRUE
    } else {
        FormulaValue.FALSE
    }
}

/**
        when(step)
        is ExecutionResult -> either pop last op from opStack or run next step if there are multiple (p&q)
        is Executable -> add step to stack and divide into further ExecutionSteps

 and
 boolsNeeded = 2
 { if(!it)
    return FALSE
   else
    return UNKNOWN


 */