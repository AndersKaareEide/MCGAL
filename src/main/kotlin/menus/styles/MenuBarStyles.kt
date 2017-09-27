package menus.styles

import tornadofx.*

class MenuBarStyles : Stylesheet() {

    companion object {
        val menubar by cssclass()
    }

    init {
        menubar {
            radioButton {
                padding = box(5.px)
            }
        }
    }
}