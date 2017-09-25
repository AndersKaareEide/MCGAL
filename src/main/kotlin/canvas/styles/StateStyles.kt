package canvas.styles

import javafx.scene.effect.DropShadow
import javafx.scene.paint.Color
import javafx.scene.paint.Paint
import tornadofx.*

class StateStyles: Stylesheet() {

    companion object {
        val hidden by cssclass()
        val focused by cssclass()
    }

    init {
        hidden {
            opacity = 0.4
        }

        focused {
            effect = DropShadow(5.0, Color.AQUA)
        }
    }
}