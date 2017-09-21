package canvas

import canvas.data.Model
import formulaParser.Formula
import formulaParser.FormulaParser
import formulaParser.GALErrorListener
import javafx.beans.property.SimpleStringProperty
import tornadofx.*

class FormulaFieldController : Controller() {

    var formula: Formula? = null
    var validating: Boolean = false

    val errorMsgProperty = SimpleStringProperty("")
    val errorListener = GALErrorListener(errorMsgProperty)

    fun validateFormString(input: String, model: Model){
        //Manipulate states directly, or
        //TODO Underline part of formula causing error or something of the like
        errorMsgProperty.value = "" //Clear error message
        try {
            val formula = FormulaParser.parse(input, errorListener)
            //TODO Make own button for checking? Also needs way to reset all states to being visible
            checkFormula(formula, model)
        } catch (e: Exception){
            errorMsgProperty.value = e.message
        }
    }

    fun checkFormula(formula: Formula, model: Model){
        for (state in model.states){
            state.hiddenProperty.set(!formula.check(state, model))
        }
        validating = true
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
}