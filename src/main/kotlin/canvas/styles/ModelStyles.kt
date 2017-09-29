package canvas.styles

import javafx.scene.effect.DropShadow
import javafx.scene.paint.Color
import tornadofx.*

class ModelStyles : Stylesheet() {

    companion object {
        val hidden by cssclass()
        val selected by cssclass()
    }

    init {
        hidden {
            opacity = 0.4
        }

        selected {
            effect = DropShadow(5.0, Color.AQUA)
        }
    }
}