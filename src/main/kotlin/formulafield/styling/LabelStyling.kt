package formulafield.styling

import javafx.scene.paint.Color
import javafx.scene.text.Font
import tornadofx.*

class LabelStyling: Stylesheet(){

    companion object {
        val debugLabel by cssclass()
    }

    init {
        debugLabel {
            font = Font(20.0)
            padding = box(0.px,3.px,0.px,0.px)
        }
    }
}