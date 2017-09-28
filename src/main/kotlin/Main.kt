import canvas.styles.StateStyles
import canvas.views.Canvas
import menus.styles.MenuBarStyles
import tornadofx.App

val styles = arrayOf(StateStyles::class, MenuBarStyles::class)

class Main : App(Canvas::class, *styles)