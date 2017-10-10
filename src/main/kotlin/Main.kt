import canvas.styles.ModelStyles
import canvas.views.Canvas
import formulafield.styling.LabelStyling
import menus.styles.MenuBarStyles
import tornadofx.App

val styles = arrayOf(ModelStyles::class, MenuBarStyles::class, LabelStyling::class)

class Main : App(Canvas::class, *styles)