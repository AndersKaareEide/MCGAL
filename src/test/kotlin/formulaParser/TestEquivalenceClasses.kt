package formulaParser

import canvas.data.AgentItem
import canvas.data.Edge
import canvas.data.State
import formulaParser.eqClassIntersectionFor
import io.ModelSerializer
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import sun.management.Agent
import java.io.File

class TestEquivalenceClasses {


    private val basicModel = ModelSerializer.deserializeModel(File("testModels/eqClassBasic.mdl"))
    private val threeAgMdl = ModelSerializer.deserializeModel(File("testModels/eqClassBasic3Agents.mdl"))

    @Test
    fun testEqClassOfStateForEmptyCoalIsSameState(){
        val s1 = basicModel.states.first()
        val expected = listOf(s1)
        assertEquals(expected, s1.eqClassIntersectionFor(listOf(), basicModel.edges))
    }

    @Test
    fun testEqClassOfStateForSingleAgent(){
        val s1 = basicModel.states.first()
        val expected = setOf(s1, basicModel.states[2])

        val agentA = basicModel.agents.first()
        assertEquals(expected, s1.eqClassIntersectionFor(listOf(agentA), basicModel.edges).toSet())
    }

    @Test
    fun testEqClassOfStateForCoalitionPerfectKnowledge(){
        val s1 = basicModel.states.first()
        val expected = setOf(s1)

        val coalition = basicModel.agents
        assertEquals(expected, s1.eqClassIntersectionFor(coalition, basicModel.edges).toSet())
    }

    @Test
    fun testEqClassOfStateForCoalitionImperfectKnowledge(){
        val s1 = threeAgMdl.states.first()
        val s3 = threeAgMdl.states[2]

        val agentA = threeAgMdl.agents[0]
        val agentC = threeAgMdl.agents[2]
        val coalition = listOf(agentA, agentC)

        val expected = setOf(s1, s3)
        assertEquals(expected, s1.eqClassIntersectionFor(coalition, basicModel.edges).toSet())
    }

    @Test
    fun testEqClassOfStateForIgnorantAgent(){
        val s1 = threeAgMdl.states.first()

        val agentC = threeAgMdl.agents[2]

        val expected = threeAgMdl.states.toSet()
        assertEquals(expected, s1.eqClassIntersectionFor(listOf(agentC), threeAgMdl.edges).toSet())
    }
}