package formulaParser

import canvas.AgentItem
import canvas.Model
import canvas.State

abstract class Formula {
    abstract fun check(state: State, model: Model): Boolean
}

abstract class BinaryOperator(val left: Formula, val right: Formula): Formula()

class Property(val propString: String): Formula() {

    override fun check(state: State, model: Model): Boolean {
        return state.props.contains(propString)
    }
}

class Negation(val inner: Formula): Formula() {

    override fun check(state: State, model: Model): Boolean {
        return inner.check(state, model).not()
    }
}

class Disjunction(left: Formula, right: Formula): BinaryOperator(left, right) {

    override fun check(state: State, model: Model): Boolean {
        return left.check(state, model) or right.check(state, model)
    }
}

class Conjunction(left: Formula, right: Formula): BinaryOperator(left, right) {

    override fun check(state: State, model: Model): Boolean {
        return left.check(state, model) and right.check(state, model)
    }
}

class Implication(left: Formula, right: Formula): BinaryOperator(left, right){
    override fun check(state: State, model: Model): Boolean {
        return !left.check(state, model) or right.check(state, model)
    }

}

class Knows(val agent: AgentItem, val inner: Formula): Formula() {
    override fun check(state: State, model: Model): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}

class Announcement(left: Formula, right: Formula): BinaryOperator(left, right){
    override fun check(state: State, model: Model): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}

class GroupAnn(val agents: List<AgentItem>, val inner: Formula): Formula() {
    override fun check(state: State, model: Model): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

}