package formulaParser

import canvas.data.AgentItem
import canvas.data.Model
import canvas.data.State
import formulaParser.formulaDebugger.*
import formulafield.FormulaLabel
import sidepanels.propertypanel.PropositionItem

//TODO Implement dualities such as <Phi>Psi
abstract class Formula(val debugger: Debugger?, val depth: Int) {
    abstract val needsParentheses: Boolean
    abstract fun check(state: State, model: Model): Boolean
    abstract fun toLabels(needsParens: Boolean = false): MutableList<FormulaLabel>
    abstract fun toExecutionSteps(state: State, model: Model): List<ExecutionStep>
    fun toFormulaItem(): FormulaItem {
        return FormulaItem(this)
    }

    fun notifyDebugger(value: FormulaValue){
        debugger?.makeEntry(this, value)
    }
}

class FormulaItem(val formula: Formula) {
    val labels: MutableList<FormulaLabel> = formula.toLabels()

    fun check(state: State, model: Model): Boolean {
        return formula.check(state, model)
    }
}

abstract class BinaryOperator(val left: Formula, val right: Formula, debugger: Debugger?, depth: Int)
    : Formula(debugger, depth) {

    override val needsParentheses = true
    abstract val opSymbol: String

    override fun toLabels(needsParens: Boolean): MutableList<FormulaLabel> {
        val left = left.toLabels(left.needsParentheses)
        val right = right.toLabels(right.needsParentheses)
        val indexRange = makeRange(needsParens, -left.size, right.size)

        left.add(FormulaLabel(this, opSymbol, indexRange))
        val result = (left + right).toMutableList()

        if (needsParens){
            insertParentheses(result, this)
        }

        return result
    }
}

class Proposition(val proposition: PropositionItem, debugger: Debugger?, depth: Int): Formula(debugger, depth) {
    override val needsParentheses = false

    override fun check(state: State, model: Model): Boolean {
        return state.props.contains(proposition)
    }

    override fun toLabels(needsParens: Boolean): MutableList<FormulaLabel> {
        return mutableListOf (FormulaLabel(this, proposition.propString, Pair(0,0)))
    }

    override fun toExecutionSteps(state: State, model: Model): List<ExecutionStep> {
        return listOf(AtomicValidationStep(proposition, state, model))
    }
}
class Negation(val inner: Formula, debugger: Debugger? = null, depth: Int): Formula() {
    override val needsParentheses = false

    override fun check(state: State, model: Model): Boolean {
        return inner.check(state, model).not()
    }

    override fun toLabels(needsParens: Boolean): MutableList<FormulaLabel> {
        val innerList = inner.toLabels(inner.needsParentheses)
        innerList.add(0, FormulaLabel(this, "¬", Pair(0, innerList.size)))
        return innerList
    }

    override fun toExecutionSteps(state: State, model: Model): List<ExecutionStep> {
        return listOf(Executable(this, 1, state, model){ it -> !it }) + inner.toExecutionSteps(state, model)
    }
}

class Disjunction(left: Formula, right: Formula): BinaryOperator(left, right) {
    override val opSymbol = "∨"

    override fun check(state: State, model: Model): Boolean {
        return left.check(state, model) or right.check(state, model)
    }

    override fun toExecutionSteps(state: State, model: Model): List<ExecutionStep> {
        return listOf()
    }
}

class Conjunction(left: Formula, right: Formula): BinaryOperator(left, right) {
    override val opSymbol = "Λ"

    override fun check(state: State, model: Model): Boolean {
        return left.check(state, model) and right.check(state, model)
    }
}

class Implication(left: Formula, right: Formula): BinaryOperator(left, right){
    override val opSymbol = "→"

    override fun check(state: State, model: Model): Boolean {
        return !left.check(state, model) or right.check(state, model)
    }

}

class Knows(val agent: AgentItem, val inner: Formula): Formula() {
    override val needsParentheses = true

    override fun check(state: State, model: Model): Boolean {
        val indishStates = getIndishStates(agent, state, model) //TODO Fix not using updated model
        if (indishStates.all { inner.check(it, model) }){
            return true
        }
        return false
    }

    override fun toLabels(needsParens: Boolean): MutableList<FormulaLabel> {
        val result = inner.toLabels(false) //K-ops always require parentheses
        insertParentheses(result, inner)
        result.add(0, FormulaLabel(this, "K${agent.name}", makeRange(needsParens,0, result.size)))
        if (needsParens){
            insertParentheses(result, this)
        }
        return (result).toMutableList()
    }
}

//TODO Look into optimizing by reusing the same updated model when checking multiple states
//TODO Fix incorrect highlighting, subformula does not get highlighted correctly
class Announcement(val announcement: Formula, val inner: Formula): Formula() {
    override val needsParentheses = true

    override fun check(state: State, model: Model): Boolean {
        if (!announcement.check(state, model)) {
            return true
        }
        else {
            val updModel = updateModel(announcement, model)
            return inner.check(state, updModel)
        }
    }

    override fun toLabels(needsParens: Boolean): MutableList<FormulaLabel> {
        val announceLabels = announcement.toLabels(false)
        val innerLabels = inner.toLabels(false)
        val sRange = makeRange(needsParens, 0,announceLabels.size + 1)
        val eRange = makeRange(needsParens, -(announceLabels.size + 1), 0)

        announceLabels.add(0, FormulaLabel(this, "[", sRange))
        announceLabels.add(FormulaLabel(this, "]", eRange))

        val result = (announceLabels + innerLabels).toMutableList()
        if (needsParens){
            insertParentheses(result, this)
        }
        return result
    }
}

//TODO Look into optimizing by caching as well
class GroupAnn(val agents: List<AgentItem>, val inner: Formula): Formula() {
    override val needsParentheses = false

    override fun check(state: State, model: Model): Boolean {
        //Formula is already true, agents simply announce Top
        if (inner.check(state, model)) {
            return true
        }

        //Atomic permanence and empty group is powerless
        if (agents.isEmpty() || !containsKnowsOp(inner)) {
            return inner.check(state, model)
        }

        val pooledModel = poolGroupKnowledge(agents, model)
        val extractProps = extractProps(inner)
        val knownProps = extractProps.filter {
            Knows(agents.first(),Proposition(it)).check(state, pooledModel)
        }

        //Update model by simulating successive announcements
        val updatedModel = knownProps.fold(model) { acc, prop -> updateModel(Proposition(prop), acc) }
        return inner.check(state, updatedModel)
    }

    override fun toLabels(needsParens: Boolean): MutableList<FormulaLabel> {
        val result = inner.toLabels(inner.needsParentheses)

        val agents = agents.joinToString { it.name }
        val label = FormulaLabel(this, "[$agents]", makeRange(needsParens,0, result.size))
        result.add(0, label)

        if (needsParens){
            insertParentheses(result, this)
        }
        return result
    }

}