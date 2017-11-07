package sidepanels.debugpanel

import canvas.controllers.CanvasController
import formulaParser.formulaDebugger.FormulaValue
import formulafield.FormulaLabel
import javafx.scene.control.Label
import sidepanels.debugpanel.DebugLabelItem
import tornadofx.*

class DebuggingLabel(val item: DebugLabelItem): FormulaLabel(item.formula, item.labelText, item.indexRange) {

    val controller = find(DebugController::class)
    val model = find(CanvasController::class).model

    init {
        textFillProperty().bind(objectBinding(item.valueProperty){
            item.valueProperty.value.color
        })


        setOnMouseEntered {
            println(item.labelText)
            println(item.value)
        }
    }

}

