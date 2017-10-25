package sidepanels.debugpanel

import canvas.data.State


import formulaParser.Formula
import formulaParser.formulaDebugger.FormulaValue
import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.SimpleObjectProperty
import tornadofx.*
/**
 * Data class used store all the information needed to create FormulaLabels
 */
open class FormulaLabelItem(val formula: Formula, val labelText: String, val indexRange: Pair<Int, Int>)

//TODO Add list of LabelItems to State and bindChildren in StateFragment
class DebugLabelItem(formula: Formula, labelText: String, indexRange: Pair<Int, Int>, val state: State,
                     var value: FormulaValue = FormulaValue.UNKNOWN): FormulaLabelItem(formula, labelText, indexRange){

    val valueProperty = SimpleObjectProperty(this,"value", value)
    val hoverProperty = SimpleBooleanProperty(this, "isHoveredOver", false)
    var isHoveredOver by hoverProperty


}
