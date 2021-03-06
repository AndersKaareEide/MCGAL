import canvas.styles.ModelStyles
import canvas.views.Canvas
import formulafield.styling.LabelStyling
import javafx.application.Application
import javafx.scene.input.KeyCode
import javafx.scene.input.KeyCodeCombination
import menus.styles.MenuBarStyles
import tornadofx.*

val styles = arrayOf(ModelStyles::class, MenuBarStyles::class, LabelStyling::class)

class Main : App(Canvas::class, *styles){
    init {
        FX.layoutDebuggerShortcut = KeyCodeCombination(KeyCode.F1)
    }
}

fun main(args: Array<String>) {
    Application.launch(Main::class.java, *args)
}
