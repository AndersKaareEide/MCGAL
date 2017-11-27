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
open class FormulaLabelItem(val formula: Formula, val labelText: String, val indexRange: IntRange)

class DebugLabelItem(formula: Formula, labelText: String, indexRange: IntRange, val state: State,
                     value: FormulaValue = FormulaValue.UNKNOWN, val isAnnouncementCheck: Boolean = false): FormulaLabelItem(formula, labelText, indexRange) {

    val valueProperty = SimpleObjectProperty(this,"value", value)
    var value by valueProperty
    val hoverProperty = SimpleBooleanProperty(this, "isHoveredOver", false)
    var isHoveredOver by hoverProperty


}
