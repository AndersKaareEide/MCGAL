import canvas.controllers.CanvasController
import formulaParser.*
import io.ModelSerializer
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import tornadofx.*
import java.io.File

class TestGroupAnnouncements {

    @Test
    fun testGroupAnnouncementOnBasicModel(){
        val filePath = "testmodels/groupAnn3StateModel.mdl"
        val model = ModelSerializer.deserializeModel(File(filePath))
        val canvasController = find(CanvasController::class)

        canvasController.loadModel(model)

        val s1 = model.states[0]
        val s2 = model.states[1]
        val s3 = model.states[2]

        assertFalse(makeNegatedGroupAnn("B","!KA(p&q)").check(s1, model, 0, null))
        assertFalse(makeNegatedGroupAnn("C","!KA(p&q)").check(s1, model, 0, null))
        assertTrue(makeNegatedGroupAnn("B,C","!KA(p&q)").check(s1, model, 0, null))

        assertFalse(model.states.any {state ->
            makeNegatedGroupAnn("A", "!(KA(p)|KA(q))").check(state, model, 0, null)
        })
    }


    private fun makeNegatedGroupAnn(coalition: String, innerFormula: String) =
            FormulaParser.parse("![$coalition]$innerFormula") { errMsg -> fail(errMsg) }
}