package menus

import canvas.controllers.CanvasController
import canvas.controllers.ClickMode
import canvas.controllers.StateController
import io.ModelSerializer
import javafx.geometry.Orientation
import javafx.scene.control.ButtonType
import javafx.stage.FileChooser
import menus.styles.MenuBarStyles
import tornadofx.*
import java.io.File

object CanvasMenuBar : View() {

    val controller: CanvasController by inject()
    val stateController: StateController by inject()

    private val fileChooser = FileChooser()

    init {
        fileChooser.extensionFilters.add(FileChooser.ExtensionFilter("Model files", "*.mdl"))
        fileChooser.initialDirectory = File(System.getProperty("user.dir") + "/models")
    }

    override val root = hbox {
        addClass(MenuBarStyles.menubar)
        menubar {
            //TODO Fix File menu being retarded and requesting focus every time Alt is pressed
            menu("_File") {
                item("_Save").action { openSaveDialog() }
                item("_Load").action { openLoadDialog() }
            }

            menu("_Edit") {
                item("_Clear Model").action {
                    confirm("Clear model", "Do you really wish to clear the model?\nThis action is irreversible",
                            ButtonType.YES, ButtonType.CANCEL) {
                        controller.clearModel()
                    }
                }
            }
        }

        separator(Orientation.VERTICAL) {  }

        //TODO Assign icons and prettify
        togglegroup {
            ClickMode.values().forEach {
                radiobutton(it.title, value = it) {
                    action { isSelected = true }
                    runLater { this.scene.accelerators.put(it.accelerator, Runnable { isSelected = true }) }
                }
            }
            bind(stateController.clickModeProperty)
        }
    }

    private fun openSaveDialog(){
        val fileToSave = fileChooser.showSaveDialog(this.currentWindow)
        if (fileToSave != null) {
            ModelSerializer.serializeModel(controller.model, fileToSave)
        }
    }

    private fun openLoadDialog() {
        val fileToLoad = fileChooser.showOpenDialog(this.currentWindow)
        if (fileToLoad != null) {
            val model = ModelSerializer.deserializeModel(fileToLoad)
            controller.loadModel(model)
        }
    }
}
