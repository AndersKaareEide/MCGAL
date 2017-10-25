package formulaParser.formulaDebugger

fun toFormulaValue(input: Boolean?): FormulaValue{
    return when(input){
        null  -> FormulaValue.UNKNOWN
        true  -> FormulaValue.TRUE
        false -> FormulaValue.FALSE
    }
}

enum class FormulaValue {
    TRUE, FALSE, UNKNOWN
}