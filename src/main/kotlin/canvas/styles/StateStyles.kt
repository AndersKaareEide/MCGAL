package canvas.styles

import tornadofx.*

class StateStyles: Stylesheet() {

    companion object {
        val hidden by cssclass()
    }

    init {
        hidden {
            opacity = 0.4
        }
    }
}