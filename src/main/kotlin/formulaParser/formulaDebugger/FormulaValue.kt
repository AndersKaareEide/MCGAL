package formulaParser.formulaDebugger

import javafx.scene.paint.Color

fun toFormulaValue(input: Boolean?): FormulaValue{
    return when(input){
        null  -> FormulaValue.UNKNOWN
        true  -> FormulaValue.TRUE
        false -> FormulaValue.FALSE
    }
}

enum class FormulaValue(val color: Color) {
    TRUE(Color.GREEN){
        override fun toString(): String { return "Y"}
    }, FALSE(Color.RED){
        override fun toString(): String { return "N"}
    }, UNKNOWN(Color.YELLOW){
        override fun toString(): String { return "?"}
    };
}