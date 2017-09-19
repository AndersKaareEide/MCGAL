package canvas

import formulaParser.Formula
import formulaParser.FormulaParser
import formulaParser.GALErrorListener
import javafx.beans.property.SimpleStringProperty
import tornadofx.*

class FormulaFieldController : Controller() {

    var formula: Formula? = null
    val errorMsgProperty = SimpleStringProperty("")
    val errorListener = GALErrorListener(errorMsgProperty)

    fun validateFormString(input: String, model: Model){
        //Manipulate states directly, or
        //TODO Underline part of formula causing error or something of the like
        errorMsgProperty.value = "" //Clear error message
        try {
            val formula = FormulaParser.parse(input, errorListener)
        } catch (e: Exception){
//            errorMsgProperty.value = e.message
        }
    }

}