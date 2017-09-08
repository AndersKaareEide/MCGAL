package dickingaround

import tornadofx.*

class MasterView: View() {

    val topView = find(TopView::class)
    val bottomView = find(BottomView::class)

    override val root = borderpane {
        top(TopView::class)
        bottom(BottomView::class)
    }

    class TopView: View() {
        override val root = label("Top View")
        lateinit var parent: MasterView
    }

    class BottomView: View() {
        override val root = label("Bottom View")
        lateinit var parent: MasterView
    }
}


