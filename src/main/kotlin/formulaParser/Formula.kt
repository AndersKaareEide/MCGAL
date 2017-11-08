package formulaParser

import canvas.data.AgentItem
import canvas.data.Model
import canvas.data.State
import formulaParser.formulaDebugger.Debugger
import formulaParser.formulaDebugger.FormulaValue
import formulaParser.formulaDebugger.toFormulaValue
import sidepanels.debugpanel.DebugLabelItem
import sidepanels.debugpanel.FormulaLabelItem
import sidepanels.propertypanel.PropositionItem

//TODO Implement dualities such as <Phi>Psi
abstract class Formula(val depth: Int) {
    abstract val needsParentheses: Boolean
    abstract fun check(state: State, model: Model, debugger: Debugger?): Boolean
    abstract fun toLabelItems(needsParens: Boolean = false): MutableList<FormulaLabelItem>
    abstract fun toDebugLabelItems(state: State, needsParens: Boolean = false): MutableMap<State, MutableList<DebugLabelItem>>
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

    override fun toDebugLabelItems(state: State, needsParens: Boolean): MutableMap<State, MutableList<DebugLabelItem>> {
        val leftMap = left.toDebugLabelItems(state, left.needsParentheses)
        val rightMap = right.toDebugLabelItems(state, right.needsParentheses)
        val indexRange = makeRange(needsParens, -leftMap.size, rightMap.size)

        leftMap[state]!!.add(DebugLabelItem(this, opSymbol, indexRange, state))
        val result = combineMapLists(leftMap, rightMap)

        if (needsParens){
            insertParentheses(result, this, state)
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
        return mutableListOf (FormulaLabelItem(this, proposition.propString, Pair(0,0)))
    }

    override fun toDebugLabelItems(state: State, needsParens: Boolean): MutableMap<State, MutableList<DebugLabelItem>> {
        val result = mutableMapOf<State, MutableList<DebugLabelItem>>()
        result.put(state, mutableListOf(DebugLabelItem(this, proposition.propString, Pair(0,0), state)))
        return result
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
        innerList.add(0, FormulaLabelItem(this, "¬", Pair(0, innerList.size)))
        return innerList
    }

    override fun toDebugLabelItems(state: State, needsParens: Boolean): MutableMap<State, MutableList<DebugLabelItem>> {
        val innerList = inner.toDebugLabelItems(state, inner.needsParentheses)
        innerList[state]!!.add(0, DebugLabelItem(this, "¬", Pair(0, innerList.size), state))
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
    override val needsParentheses = true

    override fun check(state: State, model: Model, debugger: Debugger?): Boolean {
        createDebugEntry(state, FormulaValue.UNKNOWN, debugger)
        val indishStates = getIndishStates(agent, state)
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

    override fun toDebugLabelItems(state: State, needsParens: Boolean): MutableMap<State, MutableList<DebugLabelItem>> {
        var result = mutableMapOf<State, MutableList<DebugLabelItem>>()
        val indishStates = getIndishStates(this.agent, state)

        indishStates.forEach{
            result = combineMapLists(result, inner.toDebugLabelItems(it, inner.needsParentheses))
        }

        insertParentheses(result, this, state)
        result[state]!!.add(0, DebugLabelItem(this, "K${agent.name}", makeRange(needsParens, 0 , result[state]!!.size), state))

        return result
    }
}

//TODO Look into optimizing by reusing the same updated model when checking multiple states
//TODO Fix incorrect highlighting, subformula does not get highlighted correctly
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

    override fun toDebugLabelItems(state: State, needsParens: Boolean): MutableMap<State, MutableList<DebugLabelItem>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}

//TODO Look into optimizing by caching as well
class GroupAnn(val agents: List<AgentItem>, val inner: Formula, depth: Int): Formula(depth) {
    override val needsParentheses = false

    override fun check(state: State, model: Model, debugger: Debugger?): Boolean {
        //Formula is already true, agents simply announce Top
        if (inner.check(state, model, debugger)) {
            return true
        }

        //Atomic permanence and empty group is powerless
        if (agents.isEmpty() || !containsKnowsOp(inner)) {
            return inner.check(state, model, debugger)
        }

        val pooledModel = poolGroupKnowledge(agents, model)
        val extractProps = extractProps(inner)
        val knownProps = extractProps.filter {
            //TODO Find out if depth + 1 is correct for these
            //TODO Find out how to display this perhaps, should debugger always be null so this doesn't clutter the logs?
            Knows(agents.first(),Proposition(it, depth + 2),depth + 1).check(state, pooledModel, null)
        }

        //Update model by simulating successive announcements
        //TODO Find out if these need a correct depth as well, and if it should use the debugger
        val updatedModel = knownProps.fold(model) { acc, prop -> updateModel(Proposition(prop, 0), acc, debugger) }
        return inner.check(state, updatedModel, debugger)
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

    override fun toDebugLabelItems(state: State, needsParens: Boolean): MutableMap<State, MutableList<DebugLabelItem>> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

}
