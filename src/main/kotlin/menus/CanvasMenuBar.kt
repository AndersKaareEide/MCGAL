package menus

import canvas.controllers.CanvasController
import canvas.controllers.ClickMode
import javafx.geometry.Orientation
import javafx.scene.control.ButtonType
import javafx.stage.FileChooser
import menus.styles.MenuBarStyles
import tornadofx.*
import java.io.File

object CanvasMenuBar : View() {

    val canvasController: CanvasController by inject()
    val controller: MenuBarController by inject()


    val fileChooser = FileChooser()

    init {
        fileChooser.extensionFilters.add(FileChooser.ExtensionFilter("Model files", "*.mdl"))
        fileChooser.initialDirectory = File(System.getProperty("user.dir") + "/models")
        if (!fileChooser.initialDirectory.exists()){
            fileChooser.initialDirectory.mkdir()
        }
    }

    override val root = hbox {
        addClass(MenuBarStyles.menubar)
        menubar {
            menu("_File") {
                item("_Save").action { controller.openSaveDialog(fileChooser, currentWindow) }
                item("_Load").action { controller.openLoadDialog(fileChooser, currentWindow) }
                item("_Import").action { controller.openImportDialog(fileChooser, currentWindow) }
            }

            menu("_Edit") {
                item("_Clear Model").action {
                    confirm("Clear model", "Do you really wish to clear the model?\nThis action is irreversible",
                            ButtonType.YES, ButtonType.CANCEL) {
                        canvasController.clearModel()
                    }
                }
            }
        }

        separator(Orientation.VERTICAL) {  }

        togglegroup {
            ClickMode.values().forEach {
                radiobutton(it.title, value = it) {
                    action { isSelected = true }
                    runLater { this.scene.accelerators.put(it.accelerator, Runnable { isSelected = true }) }
                }
            }
            bind(canvasController.clickModeProperty)
        }
    }


}
