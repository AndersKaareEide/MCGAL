package formulaParser.formulaDebugger

fun toFormulaValue(input: Boolean?): FormulaValue{
    return when(input){
        null  -> FormulaValue.UNKNOWN
        true  -> FormulaValue.TRUE
        false -> FormulaValue.FALSE
    }
}

enum class FormulaValue {
    TRUE{
        override fun toString(): String { return "Y"}
    }, FALSE{
        override fun toString(): String { return "N"}
    }, UNKNOWN{
        override fun toString(): String { return "?"}
    };
}