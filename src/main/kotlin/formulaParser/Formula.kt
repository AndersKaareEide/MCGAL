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
    abstract fun check(state: State, model: Model, listIndex: Int, debugger: Debugger?): Boolean
    abstract fun toLabelItems(needsParens: Boolean = false): MutableList<FormulaLabelItem>
    fun toFormulaItem(): FormulaItem {
        return FormulaItem(this)
    }

    fun createDebugEntry(state: State, value: FormulaValue, listIndex: Int, activeStates: List<State>, debugger: Debugger?){
        debugger?.makeNextEntry(this, state, listIndex, value, activeStates)
    }
}

class FormulaItem(val formula: Formula) {
    val labelItems: MutableList<FormulaLabelItem> = formula.toLabelItems()
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

    override fun check(state: State, model: Model, listIndex: Int, debugger: Debugger?): Boolean {
        val result = state.props.contains(proposition)
        createDebugEntry(state, toFormulaValue(result), listIndex, model.states, debugger)
        return result
    }

    override fun toLabelItems(needsParens: Boolean): MutableList<FormulaLabelItem> {
        return mutableListOf (FormulaLabelItem(this, proposition.propString, IntRange(0,0)))
    }
}
class Negation(val inner: Formula, depth: Int): Formula(depth) {
    override val needsParentheses = false

    override fun check(state: State, model: Model, listIndex: Int, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, listIndex, model.states, debugger)
        val result = inner.check(state, model, listIndex, debugger ).not()
        createDebugEntry(state, toFormulaValue(result), listIndex, model.states, debugger)
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

    override fun check(state: State, model: Model, listIndex: Int, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, listIndex, model.states, debugger)
        val result = left.check(state, model, listIndex, debugger) || right.check(state, model, listIndex, debugger)
        createDebugEntry(state, toFormulaValue(result), listIndex, model.states, debugger)
        return result
    }
}

class Conjunction(left: Formula, right: Formula, depth: Int): BinaryOperator(left, right, depth) {
    override val opSymbol = "Λ"

    override fun check(state: State, model: Model, listIndex: Int, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, listIndex, model.states, debugger)
        val result = left.check(state, model, listIndex, debugger) && right.check(state, model, listIndex, debugger)
        createDebugEntry(state, toFormulaValue(result), listIndex, model.states, debugger)
        return result
    }
}

class Implication(left: Formula, right: Formula, depth: Int): BinaryOperator(left, right, depth){
    override val opSymbol = "→"

    override fun check(state: State, model: Model, listIndex: Int, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, listIndex, model.states, debugger)
        val result = !left.check(state, model, listIndex, debugger) || right.check(state, model, listIndex, debugger)
        createDebugEntry(state, toFormulaValue(result), listIndex, model.states, debugger)
        return result
    }

}

class Knows(val agent: AgentItem, val inner: Formula, depth: Int): Formula(depth) {
    override val needsParentheses = false

    override fun check(state: State, model: Model, listIndex: Int, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, listIndex, model.states, debugger)
        val indishStates = getIndishStates(agent, state, model)

        val result = if (inner.check(state, model, listIndex, debugger)) {
            (indishStates - state).all {
                val index = debugger?.addNewLabelRow(it, inner) ?: 0
                inner.check(it, model, index, debugger)
            }
        } else {
            false
        }

        createDebugEntry(state, toFormulaValue(result), listIndex, model.states, debugger)
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

    override fun check(state: State, model: Model, listIndex: Int, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, listIndex, model.states, debugger)


        val result = if (!announcement.check(state, model, listIndex, null)) {
            announcement.check(state, model, listIndex, debugger) //Dirty way of preventing duplicate entries
            true
        }
        else {
            var updatedModel = model
            for (stateToCheck in model.states){
                val annIndex = debugger?.addNewLabelRow(stateToCheck, announcement) ?: 0
                if (!announcement.check(stateToCheck, updatedModel, annIndex, debugger)){
                    updatedModel = updatedModel.restrictedTo(updatedModel.states - stateToCheck)
                }
            }
            inner.check(state, updatedModel, listIndex, debugger)
        }
        createDebugEntry(state, toFormulaValue(result), listIndex, model.states, debugger)
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

class GroupAnn(val agents: List<AgentItem>, val inner: Formula, depth: Int): Formula(depth) {
    override val needsParentheses = false

    override fun check(state: State, model: Model, listIndex: Int, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, listIndex, model.states, debugger)
        val result = if (agents.isEmpty() || !containsKnowsOp(inner)) {
            //Atomic permanence and empty group is powerless
            inner.check(state, model, listIndex, debugger)
        }
        else {
            val contractedModel = bisimContract(state, model)
            val announceableExts = getAnnounceableExtensions(contractedModel, state, agents)
            announceableExts.all { announcement ->
                //TODO "Create additional entries for marking each new extension we check?"
                inner.check(state, model.restrictedTo(announcement), listIndex, debugger)
            }
        }

        createDebugEntry(state, toFormulaValue(result), listIndex, model.states, debugger)
        return result
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
