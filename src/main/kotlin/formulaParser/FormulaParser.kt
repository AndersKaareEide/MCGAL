package formulaParser

import canvas.AgentPanelController
import formulaParser.antlr.GALLexer
import formulaParser.antlr.GALParser
import org.antlr.v4.runtime.ANTLRErrorListener
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTree
import tornadofx.*

object FormulaParser : Controller() {
    val agentController: AgentPanelController by inject()

    //TODO Change Lexer to allow for agents with proper names
    fun parse(input: String, errorListener: ANTLRErrorListener): Formula {
        val lexer = GALLexer(CharStreams.fromString(input))
        lexer.addErrorListener(errorListener)
        val tokens = CommonTokenStream(lexer)
        val parser = GALParser(tokens)

        parser.addErrorListener(errorListener)

        val tree = parser.formula().form()
        return recursiveTransform(tree)
    }

    //TODO Implement handling of TokenRecognitionExceptions
    private fun recursiveTransform(tree: ParseTree): Formula {
        when (tree) {
            is GALParser.ParensFormContext ->
                return recursiveTransform(tree.inner)
            is GALParser.AtomicFormContext ->
                return Property(tree.prop.text)
            is GALParser.NegFormContext ->
                return Negation(recursiveTransform(tree.inner))
            is GALParser.ConjFormContext ->
                return Conjunction(recursiveTransform(tree.left), recursiveTransform(tree.right))
            is GALParser.DisjFormContext ->
                return Disjunction(recursiveTransform(tree.left), recursiveTransform(tree.right))
            is GALParser.ImplFormContext ->
                return Implication(recursiveTransform(tree.left), recursiveTransform(tree.right))
            is GALParser.AnnounceFormContext ->
                return Announcement(recursiveTransform(tree.announced), recursiveTransform(tree.inner))
            is GALParser.KnowsFormContext ->
                return makeKnowsFormula(tree)
            is GALParser.GroupannFormContext ->
                return makeGroupAnnouncement(tree)
        }

        throw FormulaParsingException(tree.text)
    }

    private fun makeKnowsFormula(tree: GALParser.KnowsFormContext): Formula {
        val agent = agentController.getAgent(tree.agent.text)
        return Knows(agent, recursiveTransform(tree.inner))
    }

    private fun makeGroupAnnouncement(tree: GALParser.GroupannFormContext): Formula {
        val agents = tree.agents().PROP().map { agentController.getAgent(it.text) }

        return GroupAnn(agents, recursiveTransform(tree.inner))
    }
}

class FormulaParsingException(input: String): RuntimeException("Failed to parse formula: $input")
class AgentNotFoundException(agentName: String): RuntimeException("Agent: $agentName not found in model")