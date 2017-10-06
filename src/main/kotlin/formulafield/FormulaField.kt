package formulafield

import canvas.controllers.CanvasController
import javafx.scene.input.KeyCombination
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

        val debugArea = hbox {
            
        }

        textfield {
            //TODO Clear error field when user resumes editing formula
            promptText = "Write formulas here"
            setOnAction { controller.validateFormString(text, canvasController.model) }
            accelerators.put(KeyCombination.keyCombination("Esc")) {
                controller.clearValidation(canvasController.model)
            }
            //TODO Do I want to clear the validation when the user edits the formula? Live validation perhaps?
            //Might be really slow for more complex formulas / models
//            textProperty().onChange { controller.clearValidation(edgeController.model) }
        }
    }
}
