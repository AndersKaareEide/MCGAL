package formulafield

import canvas.controllers.CanvasController
import formulaParser.Formula
import formulafield.styling.LabelStyling
import sidepanels.debugpanel.FormulaLabelItem
import tornadofx.*

class FormulaFieldLabel(formula: Formula, labelText: String, indexRange: IntRange)
    : FormulaLabel(formula, labelText, indexRange) {

    constructor(item: FormulaLabelItem) : this(item.formula, item.labelText, item.indexRange)

    val model = find(CanvasController::class).model
    val controller = find(FormulaFieldController::class)

    init {
        addClass(LabelStyling.debugLabel)

        setOnMouseEntered {
            controller.checkFormula(formula, model)
            controller.selectLabels(this, indexRange, controller.labels)
        }
        setOnMouseExited {
            controller.deselectLabels(this, indexRange)
            controller.clearValidation()
        }
    }
}

