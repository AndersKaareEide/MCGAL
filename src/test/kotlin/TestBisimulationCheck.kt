import canvas.data.AgentItem
import canvas.data.Edge.Companion.edgeBetween
import canvas.data.State
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.function.Executable
import sidepanels.propertypanel.PropositionItem

public class TestBisimulationCheck {

    @org.junit.jupiter.api.Test
    fun testIsBisimilarTo() {
        val propP = PropositionItem("p")

        val props = listOf(propP)

        val s1 = State("s1", props = props)
        val s2 = State("s2")
        val s3 = State("s3", props = props)

        val agentA = AgentItem("A")
        val agentB = AgentItem("B")


        val s1s2 = edgeBetween(s1, s2, listOf(agentA))
        val s2s3 = edgeBetween(s2, s3, listOf(agentA))
        val s1s3 = edgeBetween(s1, s3, listOf(agentA))

        assertAll(
                Executable { assertTrue(s1.isBisimilarTo(s3))  },
                Executable { assertTrue(s3.isBisimilarTo(s1))  },
                Executable { assertFalse(s1.isBisimilarTo(s2)) }
        )

        val s4 = State("s4")
        val s3s4 = edgeBetween(s3, s4, listOf(agentB))

        assertAll(
                Executable { assertFalse(s1.isBisimilarTo(s3)) }
        )

        val s5 = State("s5")
        val s1s5 = edgeBetween(s1, s5, listOf(agentB))

        assertAll(
                Executable { assertTrue(s1.isBisimilarTo(s3)) },
                Executable { assertTrue(s4.isBisimilarTo(s5)) }
        )
    }
}