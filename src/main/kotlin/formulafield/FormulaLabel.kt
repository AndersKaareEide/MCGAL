package formulafield

import canvas.controllers.CanvasController
import canvas.styles.ModelStyles
import formulaParser.Formula
import formulafield.styling.LabelStyling
import javafx.scene.control.Label
import sidepanels.debugpanel.FormulaLabelItem
import tornadofx.*

class FormulaLabel(val formula: Formula, labelText: String, val indexRange: Pair<Int, Int>): Label() {

    constructor(item: FormulaLabelItem) : this(item.formula, item.labelText, item.indexRange)

    val model = find(CanvasController::class).model
    val controller = find(FormulaFieldController::class)

    init {
        text = labelText
        addClass(LabelStyling.debugLabel)

        setOnMouseEntered {
            controller.checkFormula(formula, model)
            controller.selectLabels(this, indexRange)
        }
        setOnMouseExited {
            controller.deselectLabels(this, indexRange)
            controller.clearValidation()
        }
    }
}
