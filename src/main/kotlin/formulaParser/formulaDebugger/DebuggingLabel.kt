package formulaParser.formulaDebugger

import javafx.scene.control.Label
import sidepanels.debugpanel.DebugLabelItem

class DebuggingLabel(val item: DebugLabelItem): Label(item.labelText) {
    
}

