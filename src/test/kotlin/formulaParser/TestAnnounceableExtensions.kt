package formulaParser

import canvas.data.AgentItem
import canvas.data.Edge
import canvas.data.Model
import canvas.data.State
import io.ModelSerializer
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
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

    @Test
    fun testChainedAnnouncements(){
        val agentA = AgentItem("A")
        val agentB = AgentItem("B")
        val agentC = AgentItem("C")

        val s1 = State("s1")
        val s2 = State("s2")
        val s3 = State("s3")

        val s1s2 = Edge.makeEdgeBetween(s1, s2, listOf(agentA, agentB))
        val s1s3 = Edge.makeEdgeBetween(s1, s3, listOf(agentA, agentC))
        val s2s3 = Edge.makeEdgeBetween(s2, s3, listOf(agentA))


        val model = Model(listOf(s1, s2, s3), listOf(s1s2, s1s3, s2s3), listOf(agentA, agentB, agentC), listOf())

        val outerExtensions = getAnnounceableExtensions(model, s1, listOf(agentB))
        val restrictedModel = model.restrictedTo(outerExtensions.first { it.size == 2 })

        val innerExtensions = getAnnounceableExtensions(restrictedModel, s1, listOf(agentC))

        assertEquals(2, innerExtensions.size)
        assertTrue(innerExtensions.any { it == listOf(s1) })
        assertTrue(innerExtensions.any { it == listOf(s1, s2) })
    }
}