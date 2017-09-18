import formulaParser.*
import formulaParser.antlr.GALLexer
import formulaParser.antlr.GALParser
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTree

fun main(args: Array<String>){
    val dix = parse("[a,b0,c24]Ka12([p|q](p&q)->r34)")
    println(dix)
}

fun parse(input:String): Formula {
    val lexer = GALLexer(CharStreams.fromString(input))
    val tokens = CommonTokenStream(lexer)
    val parser = GALParser(tokens)

    val tree = parser.form()

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
        is GALParser.KnowsFormContext ->
            return Knows(tree.agent.text, recursiveTransform(tree.inner))
        is GALParser.AnnounceFormContext ->
            return Announcement(recursiveTransform(tree.announced), recursiveTransform(tree.inner))
        is GALParser.GroupannFormContext ->
            return makeGroupAnnouncement(tree)
    }

    throw RuntimeException("Error in parsing: ${tree.text}")
}

private fun makeGroupAnnouncement(tree: GALParser.GroupannFormContext): Formula {
    val agents = mutableListOf<String>()
    tree.agents().PROP().forEach { agents.add(it.text) }

    return GroupAnn(agents, recursiveTransform(tree.inner))
}
