package dickingaround

import tornadofx.*

class PopupTest : View("My View") {
    override val root = vbox {
        button("Click me") {
            action {
                openInternalWindow(PopupFragment::class)
            }
        }
    }

    class PopupFragment: Fragment(){
        override val root = label("dix")
    }
}

