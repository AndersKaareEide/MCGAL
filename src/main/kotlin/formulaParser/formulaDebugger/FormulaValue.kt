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
        override fun toString(): String { return "T"}
    }, FALSE(Color.RED){
        override fun toString(): String { return "F"}
    }, UNKNOWN(Color.BLACK){
        override fun toString(): String { return "?"}
    };
}