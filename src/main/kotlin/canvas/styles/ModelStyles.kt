package canvas.styles

import javafx.scene.effect.DropShadow
import javafx.scene.paint.Color
import tornadofx.*

class ModelStyles : Stylesheet() {

    companion object {
        val hidden by cssclass()
        val selected by cssclass()
        val accepted by cssclass()
        val rejected by cssclass()
    }

    init {
        hidden {
            opacity = 0.4
        }

        selected {
            effect = DropShadow(5.0, Color.AQUA)
        }

        accepted {
            effect = DropShadow(5.0, Color(0.259, 0.839, 0.529, 1.0))
        }

        rejected {
            effect = DropShadow(5.0, Color(0.839, 0.286, 0.259, 1.0))
        }
    }
}