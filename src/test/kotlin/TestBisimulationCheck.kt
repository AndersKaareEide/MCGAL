import canvas.data.AgentItem
import canvas.data.Edge.Companion.makeEdgeBetween
import canvas.data.State
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.function.Executable
import sidepanels.propertypanel.PropositionItem

public class TestBisimulationCheck {

    @Test
    fun testIsBisimilarTo() {
        val propP = PropositionItem("p")

        val props = listOf(propP)

        val s1 = State("s1", props = props)
        val s2 = State("s2")
        val s3 = State("s3", props = props)

        val agentA = AgentItem("A")
        val agentB = AgentItem("B")


        makeEdgeBetween(s1, s2, listOf(agentA))
        makeEdgeBetween(s2, s3, listOf(agentA))
        makeEdgeBetween(s1, s3, listOf(agentA))

        val states = mutableListOf(s1, s2, s3)

        assertAll(
                Executable { assertTrue(s1.isBisimilarTo(s3, states))  },
                Executable { assertTrue(s3.isBisimilarTo(s1, states))  },
                Executable { assertFalse(s1.isBisimilarTo(s2, states)) }
        )

        val s4 = State("s4")
        makeEdgeBetween(s3, s4, listOf(agentB))
        states.add(s4)

        assertAll(
                Executable { assertFalse(s1.isBisimilarTo(s3, states)) }
        )

        val s5 = State("s5")
        makeEdgeBetween(s1, s5, listOf(agentB))
        states.add(s5)

        assertAll(
                Executable { assertTrue(s1.isBisimilarTo(s3, states)) },
                Executable { assertTrue(s4.isBisimilarTo(s5, states)) }
        )
    }

    @Test
    fun testBisimilarityOverAgents() {
        val agentA = AgentItem("a")
        val agentB = AgentItem("b")

        val s1 = State("s1")
        val s2 = State("s2")
        val s3 = State("s3")
        val s4 = State("s4")
        val s5 = State("s5")

        val s1s2 = makeEdgeBetween(s1, s3, listOf(agentA))
        val s3s2 = makeEdgeBetween(s2, s3, listOf(agentB))
        val s4s5 = makeEdgeBetween(s4, s5, listOf(agentA, agentB))

        val states = listOf(s1, s2, s3, s4, s5)
        assertAll(
                Executable { assertTrue(s3.isBisimilarTo(s4, states)) },
                Executable { assertTrue(s3.isBisimilarTo(s5, states)) },
                Executable { assertTrue(s4.isBisimilarTo(s3, states)) },
                Executable { assertTrue(s5.isBisimilarTo(s3, states)) }
        )
    }
}