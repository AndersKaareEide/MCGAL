package formulaParser

abstract class Formula {

    abstract fun check(): Boolean

}

abstract class BinaryOperator(val left: Formula, val right: Formula): Formula()

class Property(val propString: String): Formula() {

    override fun check(): Boolean {
        return true //TODO Hook up to States and their props
    }
}

class Negation(val inner: Formula): Formula() {

    override fun check(): Boolean {
        return inner.check().not()
    }
}

class Disjunction(left: Formula, right: Formula): BinaryOperator(left, right) {

    override fun check(): Boolean {
        return left.check() or right.check()
    }
}

class Conjunction(left: Formula, right: Formula): BinaryOperator(left, right) {

    override fun check(): Boolean {
        return left.check() and right.check()
    }
}

class Implication(left: Formula, right: Formula): BinaryOperator(left, right){
    override fun check(): Boolean {
        return !left.check() or right.check()
    }

}

class Knows(val agent: String, val inner: Formula): Formula() {
    override fun check(): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}

class Announcement(left: Formula, right: Formula): BinaryOperator(left, right){
    override fun check(): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}

class GroupAnn(val agents: List<String>, val inner: Formula): Formula() {
    override fun check(): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

}