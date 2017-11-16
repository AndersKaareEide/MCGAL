package formulafield

import canvas.controllers.CanvasController
import canvas.data.Model
import canvas.styles.ModelStyles
import formulaParser.Formula
import formulaParser.FormulaParser
import formulaParser.FormulaParsingException
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
    lateinit var labels: List<FormulaFieldLabel>

    val errorMsgProperty = SimpleStringProperty("")

    fun validateFormString(input: String, debugArea: HBox){
        //TODO Underline part of formula causing error or something of the like
        errorMsgProperty.value = "" //Clear error message
        try {
            val formula = FormulaParser.parse(input, errorMsgProperty)

            //TODO Limit length of list or something
            if (!formulaList.contains(input)) {
                formulaList.add(input)
            }

            checkFormula(formula, canvasController.model)

            clearLabelListeners(debugArea)
            //TODO Clear labels when the model is edited
            labels = formula.toFormulaItem().labelItems.map { FormulaFieldLabel(it) }
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
            state.validationStyle = if (formula.check(state,model, null)){
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
    //TODO Move out into LabelController
    fun selectLabels(label: FormulaLabel, range: IntRange, labels: List<FormulaLabel>){
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
}