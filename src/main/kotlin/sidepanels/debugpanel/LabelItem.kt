package sidepanels.debugpanel

import canvas.data.State
import formulaParser.Formula
import formulaParser.formulaDebugger.FormulaValue
import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.SimpleObjectProperty
import tornadofx.*

class LabelItem(val state: State, val formula: Formula, var value: FormulaValue = FormulaValue.UNKNOWN){

    val valueProperty = SimpleObjectProperty(this,"value", value)

    val hoverProperty = SimpleBooleanProperty(this, "isHoveredOver", false)
    var isHoveredOver by hoverProperty
}