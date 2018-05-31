package formulaParser

import io.ModelSerializer
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.io.File

class TestAnnounceableExtensions {



    @Test
    fun testBasic3StateModel(){
        val model = ModelSerializer.deserializeModel(File("testmodels/extensionsBasic.mdl"))

        val agentA = model.agents[0]
        val agentB = model.agents[1]

        val s1 = model.states[0]
        val s2 = model.states[1]
        val s3 = model.states[2]

        var annableExtensions = getAnnounceableExtensions(model, s1, listOf(agentA))
        var expected = listOf(listOf(s1, s2), listOf(s1, s2, s3))

        assertEquals(expected, annableExtensions)

        annableExtensions = getAnnounceableExtensions(model, s1, listOf(agentA, agentB))
        expected = listOf(listOf(s1), listOf(s1, s2), listOf(s1, s3), listOf(s1, s2, s3))

        assertEquals(expected, annableExtensions)
    }
}