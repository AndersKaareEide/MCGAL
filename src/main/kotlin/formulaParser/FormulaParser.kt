package formulaParser

import formulaParser.antlr.GALLexer
import formulaParser.antlr.GALParser
import sidepanels.agentpanel.AgentPanelController
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTree
import sidepanels.propertypanel.PropPanelController
import tornadofx.*

object FormulaParser : Controller() {
    val agentController: AgentPanelController by inject()
    val propController: PropPanelController by inject()

    fun parse(input: String, errorMessageCallBack: (String) -> Unit): Formula {
        val errorListener = GALErrorListener(errorMessageCallBack)

        val lexer = GALLexer(CharStreams.fromString(input))
        lexer.addErrorListener(errorListener)
        val tokens = CommonTokenStream(lexer)
        val parser = GALParser(tokens)

        parser.addErrorListener(errorListener)

        val tree = parser.formula().form()
        return recursiveTransform(tree, 0)
    }

    //TODO Implement handling of TokenRecognitionExceptions
    private fun recursiveTransform(tree: ParseTree, depth: Int): Formula {
        when (tree) {
            is GALParser.ParensFormContext ->
                return recursiveTransform(tree.inner, depth)
            is GALParser.AtomicFormContext ->
                return Proposition(propController.getProposition(tree.prop.text), depth)
            is GALParser.NegFormContext ->
                return Negation(recursiveTransform(tree.inner, depth +1), depth)
            is GALParser.ConjFormContext ->
                return Conjunction(recursiveTransform(tree.left, depth + 1), recursiveTransform(tree.right, depth +1 ), depth)
            is GALParser.DisjFormContext ->
                return Disjunction(recursiveTransform(tree.left, depth + 1), recursiveTransform(tree.right, depth + 1), depth)
            is GALParser.ImplFormContext ->
                return Implication(recursiveTransform(tree.left, depth + 1), recursiveTransform(tree.right, depth + 1), depth)
            is GALParser.AnnounceFormContext ->
                //TODO Determine whether to give the announced and the inner formulas of announcements the same depth
                return Announcement(recursiveTransform(tree.announced, depth + 1), recursiveTransform(tree.inner, depth + 2), depth)
            is GALParser.KnowsFormContext ->
                return makeKnowsFormula(tree, depth)
            is GALParser.GroupannFormContext ->
                return makeGroupAnnouncement(tree, depth)
        }

        throw FormulaParsingException(tree.text)
    }

    private fun makeKnowsFormula(tree: GALParser.KnowsFormContext, depth: Int): Formula {
        val agent = agentController.getAgent(tree.agent.text)
        return Knows(agent, recursiveTransform(tree.inner, depth + 1), depth)
    }

    private fun makeGroupAnnouncement(tree: GALParser.GroupannFormContext, depth: Int): Formula {
        val agents = tree.agents().AGENT().map { agentController.getAgent(it.text) }

        return GroupAnn(agents, recursiveTransform(tree.inner, depth + 1), depth)
    }
}

open class FormulaParsingException(input: String): RuntimeException("Failed to parse formula: $input")
class AgentNotFoundException(agentName: String): FormulaParsingException("Agent: $agentName not found in model")
class PropertyNotFoundException(propString: String): FormulaParsingException("Property: $propString not found in model")