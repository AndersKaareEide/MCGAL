package formulaParser.formulaDebugger

fun toFormulaValue(input: Boolean?): FormulaValue{
    when(input){
        null  -> FormulaValue.UNKNOWN
        true  -> FormulaValue.TRUE
        false -> FormulaValue.FALSE
    }
}

enum class FormulaValue {
    TRUE, FALSE, UNKNOWN
}