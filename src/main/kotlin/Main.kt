import canvas.styles.ModelStyles
import canvas.views.Canvas
import menus.styles.MenuBarStyles
import tornadofx.App

val styles = arrayOf(ModelStyles::class, MenuBarStyles::class)

class Main : App(Canvas::class, *styles)