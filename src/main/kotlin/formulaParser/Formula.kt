package formulaParser

import bisimContract
import canvas.data.AgentItem
import canvas.data.Model
import canvas.data.State
import formulaParser.formulaDebugger.Debugger
import formulaParser.formulaDebugger.FormulaValue
import formulaParser.formulaDebugger.toFormulaValue
import sidepanels.debugpanel.FormulaLabelItem
import sidepanels.propertypanel.PropositionItem

abstract class Formula(val depth: Int) {
    abstract val needsParentheses: Boolean
    abstract fun check(state: State, model: Model, debugger: Debugger?): Boolean
    abstract fun toLabelItems(needsParens: Boolean = false): MutableList<FormulaLabelItem>
    fun toFormulaItem(): FormulaItem {
        return FormulaItem(this)
    }

    fun createDebugEntry(state: State, value: FormulaValue, debugger: Debugger?){
        debugger?.makeNextEntry(this, state, value)
    }
}

class FormulaItem(val formula: Formula) {
    val labelItems: MutableList<FormulaLabelItem> = formula.toLabelItems()

    fun check(state: State, model: Model, debugger: Debugger?): Boolean {
        return formula.check(state, model, debugger)
    }
}

abstract class BinaryOperator(val left: Formula, val right: Formula, depth: Int) : Formula(depth) {

    override val needsParentheses = true
    abstract val opSymbol: String

    override fun toLabelItems(needsParens: Boolean): MutableList<FormulaLabelItem> {
        val leftList = left.toLabelItems(left.needsParentheses)
        val rightList = right.toLabelItems(right.needsParentheses)
        val indexRange = makeRange(needsParens, -leftList.size, rightList.size)

        leftList.add(FormulaLabelItem(this, opSymbol, indexRange))
        val result = (leftList + rightList).toMutableList()

        if (needsParens){
            insertParentheses(result, this)
        }

        return result
    }
}

class Proposition(val proposition: PropositionItem, depth: Int): Formula(depth) {
    override val needsParentheses = false

    override fun check(state: State, model: Model, debugger: Debugger?): Boolean {
        val result = state.props.contains(proposition)
        createDebugEntry(state, toFormulaValue(result), debugger)
        return result
    }

    override fun toLabelItems(needsParens: Boolean): MutableList<FormulaLabelItem> {
        return mutableListOf (FormulaLabelItem(this, proposition.propString, IntRange(0,0)))
    }
}
class Negation(val inner: Formula, depth: Int): Formula(depth) {
    override val needsParentheses = false

    override fun check(state: State, model: Model, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, debugger)
        val result = inner.check(state, model, debugger).not()
        createDebugEntry(state, toFormulaValue(result), debugger)
        return result
    }

    override fun toLabelItems(needsParens: Boolean): MutableList<FormulaLabelItem> {
        val innerList = inner.toLabelItems(inner.needsParentheses)
        innerList.add(0, FormulaLabelItem(this, "¬", IntRange(0, innerList.size)))
        return innerList
    }
}

class Disjunction(left: Formula, right: Formula, depth: Int): BinaryOperator(left, right, depth) {
    override val opSymbol = "∨"

    override fun check(state: State, model: Model, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, debugger)
        val result = left.check(state, model, debugger) || right.check(state, model, debugger)
        createDebugEntry(state, toFormulaValue(result), debugger)
        return result
    }
}

class Conjunction(left: Formula, right: Formula, depth: Int): BinaryOperator(left, right, depth) {
    override val opSymbol = "Λ"

    override fun check(state: State, model: Model, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, debugger)
        val result = left.check(state, model, debugger) && right.check(state, model, debugger)
        createDebugEntry(state, toFormulaValue(result), debugger)
        return result
    }
}

class Implication(left: Formula, right: Formula, depth: Int): BinaryOperator(left, right, depth){
    override val opSymbol = "→"

    override fun check(state: State, model: Model, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, debugger)
        val result = !left.check(state, model, debugger) || right.check(state, model, debugger)
        createDebugEntry(state, toFormulaValue(result), debugger)
        return result
    }

}

class Knows(val agent: AgentItem, val inner: Formula, depth: Int): Formula(depth) {
    override val needsParentheses = false

    override fun check(state: State, model: Model, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, debugger)
        val indishStates = getIndishStates(agent, state, model)
        val result = indishStates.all { inner.check(it, model, debugger) }
        createDebugEntry(state, toFormulaValue(result), debugger)
        return result

    }
    override fun toLabelItems(needsParens: Boolean): MutableList<FormulaLabelItem> {
        val result = inner.toLabelItems(false) //K-ops parentheses are required by formula syntax
        insertParentheses(result, inner)
        result.add(0, FormulaLabelItem(this, "K${agent.name}", makeRange(needsParens,0, result.size)))
        if (needsParens){
            insertParentheses(result, this)
        }
        return (result).toMutableList()
    }
}

class Announcement(val announcement: Formula, val inner: Formula, depth: Int): Formula(depth) {
    override val needsParentheses = true

    override fun check(state: State, model: Model, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, debugger)
        val result = if (!announcement.check(state, model, null)) {
            announcement.check(state, model, debugger) //Dirty way of preventing duplicate entries
            true
        }
        else {
            val updModel = updateModel(announcement, model, debugger)
            inner.check(state, updModel, debugger)
        }
        createDebugEntry(state, toFormulaValue(result), debugger)
        return result
    }

    override fun toLabelItems(needsParens: Boolean): MutableList<FormulaLabelItem> {
        val announceLabels = announcement.toLabelItems(false)
        val innerLabels = inner.toLabelItems(false)
        val sRange = makeRange(needsParens, 0,announceLabels.size + 1 + innerLabels.size)
        val eRange = makeRange(needsParens, -(announceLabels.size + 1), innerLabels.size)

        announceLabels.add(0, FormulaLabelItem(this, "[", sRange))
        announceLabels.add(FormulaLabelItem(this, "]", eRange))

        val result = (announceLabels + innerLabels).toMutableList()
        if (needsParens){
            insertParentheses(result, this)
        }
        return result
    }
}

// M,s |= [G]F iff for every announcement in Anns(G,M,s), M,s |= [announcement]F
// M,s |= <G>F iff there exists an announcement such that M,s |= announcement
//        and M,s|announcement |= F
class GroupAnn(val agents: List<AgentItem>, val inner: Formula, depth: Int): Formula(depth) {
    override val needsParentheses = false

    override fun check(state: State, model: Model, debugger: Debugger?): Boolean {
        val result = if (agents.isEmpty() || !containsKnowsOp(inner)) {
            //Atomic permanence and empty group is powerless
            inner.check(state, model, debugger)
        }
        else {
            //TODO "Implement generation of labeling formulas and bisimulation contraction"
            val pair = bisimContract(model)
            val contractedModel = pair.first
            val filteredStateMapping = pair.second

            val contractedState = filteredStateMapping[state] ?: state
            val announceableExts = getAnnounceableExtensions(contractedModel, contractedState, agents)
            announceableExts.all { announcement ->
                inner.check(contractedState, model.restrictedTo(announcement), debugger) //Don't use debugger, would look funny since states are filtered
            }
        }

        createDebugEntry(state, toFormulaValue(result), debugger)
        return result
        /** TODO Stuff below
         * Restrict model in similar fashion
         * Create debug entry for bisimulation contraction, shading out any filtered states similarly to how announcements are visualized
         * Restriction func returns both filtered set of states as well as mapping for each filtered state to its bisimilar state
         * If state got filtered, use mapping[state] instead
         */
    }

    override fun toLabelItems(needsParens: Boolean): MutableList<FormulaLabelItem> {
        val result = inner.toLabelItems(inner.needsParentheses)

        val agents = agents.joinToString { it.name }
        val label = FormulaLabelItem(this, "[$agents]", makeRange(needsParens,0, result.size))
        result.add(0, label)

        if (needsParens){
            insertParentheses(result, this)
        }
        return result
    }
}
