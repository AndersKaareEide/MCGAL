package sidepanels.debugpanel

import canvas.controllers.CanvasController
import formulaParser.formulaDebugger.FormulaValue
import formulafield.FormulaLabel
import javafx.scene.control.Label
import sidepanels.debugpanel.DebugLabelItem
import tornadofx.*

class DebugLabel(val item: DebugLabelItem): FormulaLabel(item.formula, item.labelText, item.indexRange) {

    val controller = find(DebugController::class)
    val model = find(CanvasController::class).model

    init {
        textFillProperty().bind(objectBinding(item.valueProperty){
            item.valueProperty.value.color
        })

        setOnMouseEntered {
            //TODO Add mouse listeners that make the formula light up
        }
    }
}

