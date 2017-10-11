package formulafield

import canvas.controllers.CanvasController
import javafx.beans.value.ChangeListener
import javafx.scene.input.KeyCode
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

        val debugArea = hbox()

        val inputField = textfield {
            //TODO Clear error field when user resumes editing formula
            promptText = "Write formulas here"
            setOnAction { controller.validateFormString(text, canvasController.model, debugArea) }
            accelerators.put(KeyCombination.keyCombination("Esc")) {
                controller.clearValidation()
            }
            focusedProperty().addListener({ _, _, it -> if(!it) { controller.clearValidation() } })

            setOnKeyPressed {
                if (it.code == KeyCode.UP) {
                    controller.getPreviousFormula(this)
                    it.consume()
                } else if (it.code == KeyCode.DOWN) {
                    controller.getNextFormula(this)
                    it.consume()
                }
            }
        }

        setOnKeyPressed {
            if (it.code == KeyCode.UP) {
                controller.getPreviousFormula(inputField)
                it.consume()
            } else if (it.code == KeyCode.DOWN) {
                controller.getNextFormula(inputField)
                it.consume()
            }
        }
    }
}