package formulafield

import canvas.controllers.CanvasController
import canvas.styles.ModelStyles
import javafx.collections.FXCollections
import javafx.scene.input.KeyCombination
import javafx.scene.text.Font
import tornadofx.*

class FormulaField : View("My View") {

    val canvasController: CanvasController by inject()
    val controller: FormulaFieldController by inject()

    override val root = vbox {
        //Label used to display error messages
        label(controller.errorMsgProperty) {
            removeWhen {
                textProperty().isEmpty
            }
        }

        val debugArea = hbox()

        textfield {
            //TODO Clear error field when user resumes editing formula
            promptText = "Write formulas here"
            setOnAction { controller.validateFormString(text, canvasController.model, debugArea) }
            accelerators.put(KeyCombination.keyCombination("Esc")) {
                controller.clearValidation(canvasController.model)
            }
        }
    }
}
