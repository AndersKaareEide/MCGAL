package sidepanels.debugpanel

import canvas.data.State
import formulaParser.Formula
import formulaParser.formulaDebugger.FormulaValue
import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.SimpleObjectProperty
import tornadofx.*


//TODO Add list of LabelItems to State and bindChildren in StateFragment
class DebugLabelItem(val state: State, val labelText: String, var value: FormulaValue = FormulaValue.UNKNOWN){

    val valueProperty = SimpleObjectProperty(this,"value", value)

    val hoverProperty = SimpleBooleanProperty(this, "isHoveredOver", false)
    var isHoveredOver by hoverProperty
}


/**
 * Data class used store all the information needed to create DebuggingLabels
 */
data class FormulaChunk(val labelText: String, val indexRange: Pair<Int, Int>)