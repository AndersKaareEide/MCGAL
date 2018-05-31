package formulaParser

import canvas.data.State
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class TestPowerSetGenerator {
    @Test
    fun testGenerator() {
        val s1 = State("s1")
        val s2 = State("s2")
        val s3 = State("s3")

        assertEquals(listOf(listOf(), listOf(s1)),generatePowerSetOfStates(listOf(s1)))

        val expected = listOf(listOf(), listOf(s1), listOf(s2), listOf(s3),
                listOf(s1, s2), listOf(s1, s3), listOf(s2, s3), listOf(s1, s2, s3))

        assertEquals(expected, generatePowerSetOfStates(listOf(s1, s2, s3)))
    }
}