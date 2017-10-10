package menus

import canvas.controllers.CanvasController
import canvas.data.Model
import io.ModelSerializer
import javafx.stage.FileChooser
import javafx.stage.Window
import tornadofx.*

class MenuBarController : Controller() {

    val canvasController: CanvasController by inject()

    fun openSaveDialog(fileChooser: FileChooser, currentWindow: Window?){
        val fileToSave = fileChooser.showSaveDialog(currentWindow)
        if (fileToSave != null) {
            ModelSerializer.serializeModel(canvasController.model, fileToSave)
        }
    }

    fun openLoadDialog(fileChooser: FileChooser, currentWindow: Window?) {
        val model = loadModelFromFile(fileChooser, currentWindow)
        if (model != null) {
            canvasController.loadModel(model)
        }
    }

    fun openImportDialog(fileChooser: FileChooser, currentWindow: Window?) {
        val model = loadModelFromFile(fileChooser, currentWindow)
        if (model != null){
            canvasController.importModel(model)
        }
    }

    private fun loadModelFromFile(fileChooser: FileChooser, currentWindow: Window?): Model? {
        //TODO Handle file reading errors gracefully (User tries to load invalid file ect.)
        val fileToLoad = fileChooser.showOpenDialog(currentWindow)
        return if (fileToLoad != null) {
            ModelSerializer.deserializeModel(fileToLoad)
        } else {
            null
        }
    }
}