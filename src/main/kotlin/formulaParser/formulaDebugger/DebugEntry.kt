package formulaParser.formulaDebugger

import canvas.data.State
import formulafield.FormulaLabel
import javafx.scene.layout.HBox
import sidepanels.debugpanel.DebugLabelItem

class DebugEntry(val state: State, val labels: List<FormulaLabel>, val value: FormulaValue,
                 val valuationMap: Map<DebugLabelItem, FormulaValue>, val depth: Int,
                 val activeStates: List<State>){



    val labelbox: HBox = HBox()
    val stateNameProp = state.nameProperty

    init {
        labelbox.children.addAll(labels)
    }
}

enum class EntryType {
    PlainEntry, AnnouncementEntry
}