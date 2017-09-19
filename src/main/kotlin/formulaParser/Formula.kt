package formulaParser

import canvas.AgentItem
import canvas.Model

abstract class Formula {
    abstract fun check(model: Model): Boolean
}

abstract class BinaryOperator(val left: Formula, val right: Formula): Formula()

class Property(val propString: String): Formula() {

    override fun check(model: Model): Boolean {
        return true //TODO Hook up to States and their props
    }
}

class Negation(val inner: Formula): Formula() {

    override fun check(model: Model): Boolean {
        return inner.check(model).not()
    }
}

class Disjunction(left: Formula, right: Formula): BinaryOperator(left, right) {

    override fun check(model: Model): Boolean {
        return left.check(model) or right.check(model)
    }
}

class Conjunction(left: Formula, right: Formula): BinaryOperator(left, right) {

    override fun check(model: Model): Boolean {
        return left.check(model) and right.check(model)
    }
}

class Implication(left: Formula, right: Formula): BinaryOperator(left, right){
    override fun check(model: Model): Boolean {
        return !left.check(model) or right.check(model)
    }

}

class Knows(val agent: AgentItem, val inner: Formula): Formula() {
    override fun check(model: Model): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}

class Announcement(left: Formula, right: Formula): BinaryOperator(left, right){
    override fun check(model: Model): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}

class GroupAnn(val agents: List<AgentItem>, val inner: Formula): Formula() {
    override fun check(model: Model): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

}