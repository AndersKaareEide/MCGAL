package menus

import canvas.controllers.CanvasController
import io.ModelSerializer
import javafx.stage.FileChooser
import tornadofx.*
import java.io.File

object CanvasMenuBar : View() {

    val controller: CanvasController by inject()
    val fileChooser = FileChooser()

    init {
        fileChooser.extensionFilters.add(FileChooser.ExtensionFilter("Model files", "*.mdl"))
        fileChooser.initialDirectory = File(System.getProperty("user.dir") + "/models")
    }

    override val root = menubar {
        menu("File") {
            item("Save").action { openSaveDialog() }
            item("Load").action { openLoadDialog() }
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
