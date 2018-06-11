package formulafield

import canvas.controllers.CanvasController
import canvas.data.Model
import canvas.styles.ModelStyles
import formulaParser.Formula
import formulaParser.FormulaParser
import formulaParser.FormulaParsingException
import javafx.beans.property.SimpleStringProperty
import javafx.collections.FXCollections
import javafx.scene.control.TextField
import org.antlr.v4.runtime.RecognitionException
import tornadofx.*
import java.util.*

class FormulaFieldController : Controller() {

    private val canvasController: CanvasController by inject()

    var forwards: Boolean? = null
    var validating: Boolean = false
    val formulaList: LinkedList<String> = LinkedList()
    val labels = FXCollections.observableArrayList<FormulaFieldLabel>()!!

    val errorMsgProperty = SimpleStringProperty("")

    fun validateFormString(input: String){
        errorMsgProperty.value = "" //Clear error message
        try {
            val formula = FormulaParser.parse(input, this::setErrorMsg)

            if (!formulaList.contains(input)) {
                formulaList.add(input)
            }

            checkFormula(formula, canvasController.model)

            clearLabelListeners()
            labels.setAll(formula.toFormulaItem().labelItems.map { FormulaFieldLabel(it) })

        } catch (e: RecognitionException){
            setErrorMsg(e.message!!)
        } catch (e: FormulaParsingException){
            setErrorMsg(e.message!!)
        } catch (e: IllegalStateException){
            setErrorMsg("Error parsing agents in group announcement")
        }
    }

    fun clearLabels(){
        labels.clear()
        clearLabelListeners()
    }

    private fun clearLabelListeners() {
        labels.forEach {
            it.setOnMouseExited  {}
            it.setOnMouseEntered {}
        }
    }

    fun checkFormula(formula: Formula, model: Model){
        for (state in model.states){
            state.validationStyle = if (formula.check(state, model, 0, null )){
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

    fun selectLabels(label: FormulaLabel, range: IntRange){
        getLabels(label, range, labels).forEach {
            it.addClass(ModelStyles.selected)
        }
    }

    fun getLabels(label: FormulaLabel, relativeRange: IntRange, labels: List<FormulaLabel>): List<FormulaLabel> {
        val opIndex = labels.indexOf(label)
        val absRange = IntRange(relativeRange.first + opIndex, relativeRange.last + opIndex)

        return labels.slice(absRange)
    }

    fun deselectLabels(label: FormulaLabel, range: IntRange) {
        val opIndex = labels.indexOf(label)
        for (index in opIndex + range.first .. opIndex + range.last){
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

    fun setErrorMsg(errorMsg: String){
        errorMsgProperty.set(errorMsg)
    }

    fun clearErrorMsg(){
        errorMsgProperty.set("")
    }
}