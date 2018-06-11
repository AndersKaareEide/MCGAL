package sidepanels.debugpanel

import formulafield.FormulaLabel
import tornadofx.*

class DebugLabel(val item: DebugLabelItem): FormulaLabel(item.formula, item.labelText, item.indexRange) {

    init {
        textFillProperty().bind(objectBinding(item.valueProperty){
            item.valueProperty.value.color
        })
    }
}

