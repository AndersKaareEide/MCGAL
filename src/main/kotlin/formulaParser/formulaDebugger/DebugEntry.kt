package formulaParser.formulaDebugger

import canvas.data.State
import formulaParser.Formula
import formulafield.FormulaLabel
import javafx.scene.layout.HBox

//TODO Add edge traversal as a validation step for visual purposes?
class DebugEntry(val state: State, val labels: List<FormulaLabel>, val value: FormulaValue,
                 val formValues: Map<Pair<State, Formula>, FormulaValue>, val depth: Int){

    val labelbox: HBox = HBox()
    val stateNameProp = state.nameProperty

    init {
        labelbox.children.addAll(labels)
    }
}