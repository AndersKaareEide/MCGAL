package formulafield

import canvas.controllers.CanvasController
import canvas.data.Model
import canvas.styles.ModelStyles
import formulaParser.Formula
import formulaParser.FormulaParser
import formulaParser.FormulaParsingException
import formulaParser.GALErrorListener
import formulafield.styling.LabelStyling
import javafx.beans.property.SimpleStringProperty
import javafx.scene.layout.HBox
import org.antlr.v4.runtime.RecognitionException
import tornadofx.*

class FormulaFieldController : Controller() {

    val canvasController: CanvasController by inject()

    var formula: Formula? = null
    var validating: Boolean = false
    lateinit var labels: MutableList<FormulaLabel>

    val errorMsgProperty = SimpleStringProperty("")
    val errorListener = GALErrorListener(errorMsgProperty)

    fun validateFormString(input: String, model: Model, debugArea: HBox){
        //TODO Underline part of formula causing error or something of the like
        errorMsgProperty.value = "" //Clear error message
        try {
            val formula = FormulaParser.parse(input, errorListener)
            checkFormula(formula, model)

            // Clear listeners from old labels in case the user is still hovering over one as it is removed
            debugArea.children.forEach {
                it.setOnMouseExited {}
                it.setOnMouseEntered {}
            }

            labels = formula.toFormulaItem().labels
            debugArea.children.setAll(labels)

        } catch (e: RecognitionException){
            errorMsgProperty.value = e.message
        } catch (e: FormulaParsingException){
            errorMsgProperty.value = e.message
        }
    }

    fun checkFormula(formula: Formula, model: Model){
        for (state in model.states){
            //TODO Replace with green / red glow instead
            state.hiddenProperty.set(!formula.check(state, model))
        }
        validating = true
        canvasController.clearSelectedComponents()
    }

    /**
     * Makes all states hidden again
     */
    fun clearValidation(model: Model) {
        if (validating) {
            for (state in model.states) {
                state.hiddenProperty.set(false)
            }
            validating = false
        }
    }

    fun selectLabels(label: FormulaLabel, range: Pair<Int, Int>){
        val opIndex = labels.indexOf(label)
        for (index in opIndex + range.first .. opIndex + range.second){
            labels[index].addClass(ModelStyles.selected)
        }
    }

    fun deselectLabels(label: FormulaLabel, range: Pair<Int, Int>) {
        val opIndex = labels.indexOf(label)
        for (index in opIndex + range.first .. opIndex + range.second){
            labels[index].removeClass(ModelStyles.selected)
        }
    }
}