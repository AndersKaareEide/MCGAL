package dickingaround

import javafx.geometry.Point2D
import tornadofx.*
import tornadofx.ViewTransition.Direction

class ViewSwitchingTest : View("View switching test") {
    override val root = vbox {
        button("Switch to view 2") {
            action {
                replaceWith(View2::class, ViewTransition.Slide(0.3.seconds, Direction.LEFT))
            }
        }
    }

    override fun onDock() {
        println("Docking MyView1!")
    }

    override fun onUndock() {
        println("Undocking MyView1!")
    }

    class View2 : View() {
        override val root = vbox {
            button("Switch to view 1") {
                action {
                    replaceWith(ViewSwitchingTest::class, ViewTransition.Explode(1.seconds, Point2D(3.0,3.0)))
                }
            }
        }
    }
}
