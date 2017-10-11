package formulafield

import canvas.controllers.CanvasController
import canvas.data.Model
import canvas.styles.ModelStyles
import formulaParser.Formula
import formulaParser.FormulaParser
import formulaParser.FormulaParsingException
import formulaParser.GALErrorListener
import javafx.beans.property.SimpleStringProperty
import javafx.scene.control.TextField
import javafx.scene.layout.HBox
import org.antlr.v4.runtime.RecognitionException
import tornadofx.*
import java.util.*

class FormulaFieldController : Controller() {

    val canvasController: CanvasController by inject()

    var forwards: Boolean? = null
    var validating: Boolean = false
    val formulaList: LinkedList<String> = LinkedList()
    lateinit var labels: MutableList<FormulaLabel>

    val errorMsgProperty = SimpleStringProperty("")
    val errorListener = GALErrorListener(errorMsgProperty)

    fun validateFormString(input: String, model: Model, debugArea: HBox){
        //TODO Underline part of formula causing error or something of the like
        errorMsgProperty.value = "" //Clear error message
        try {
            val formula = FormulaParser.parse(input, errorListener)

            //TODO Limit length of list or something
            if (!formulaList.contains(input)) {
                formulaList.add(input)
            }

            checkFormula(formula, model)

            clearLabelListeners(debugArea)
            //TODO Clear labels when the model is edited
            labels = formula.toFormulaItem().labels
            debugArea.children.setAll(labels)

        } catch (e: RecognitionException){
            errorMsgProperty.value = e.message
        } catch (e: FormulaParsingException){
            errorMsgProperty.value = e.message
        } catch (e: IllegalStateException){
            errorMsgProperty.value = "Error parsing agents in group announcement"
        }
    }

    private fun clearLabelListeners(debugArea: HBox) {
        debugArea.children.forEach {
            it.setOnMouseExited  {}
            it.setOnMouseEntered {}
        }
    }

    fun checkFormula(formula: Formula, model: Model){
        for (state in model.states){
            state.validationStyle = if (formula.check(state,model)){
                ModelStyles.accepted
            } else {
                ModelStyles.rejected
            }
        }
        validating = true
        canvasController.clearSelectedComponents()
    }

    /**
     * Removes all validation-related styling from states
     */
    fun clearValidation() {
        if (validating) {
            for (state in canvasController.model.states) {
                state.validationStyle = null
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

    fun getNextFormula(inputField: TextField) {
        if (forwards != null && !forwards!!){
            formulaList.addLast(formulaList.pollFirst())
        }
        if (!formulaList.isEmpty()) {
            val next = formulaList.pollFirst()
            formulaList.addLast(next)
            inputField.text = next
            forwards = true
        }
    }

    fun getPreviousFormula(inputField: TextField) {
        if (forwards != null && forwards!!){
            formulaList.addFirst(formulaList.pollLast())
        }
        if (!formulaList.isEmpty()) {
            val next = formulaList.pollLast()
            formulaList.addFirst(next)
            inputField.text = next
            forwards = false
        }
    }
}