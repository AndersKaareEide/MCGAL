package formulafield

import canvas.controllers.CanvasController
import canvas.styles.ModelStyles
import formulaParser.Formula
import formulafield.styling.LabelStyling
import javafx.scene.control.Label
import sidepanels.debugpanel.FormulaLabelItem
import tornadofx.*

open class FormulaLabel(val formula: Formula, labelText: String, val indexRange: Pair<Int, Int>): Label(labelText) {

    constructor(item: FormulaLabelItem) : this(item.formula, item.labelText, item.indexRange)
}
