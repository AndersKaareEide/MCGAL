package formulaParser

import canvas.AgentItem
import canvas.Model
import canvas.State

//TODO Implement dualities such as <Phi>Psi
abstract class Formula {
    abstract fun check(state: State, model: Model): Boolean
}

abstract class BinaryOperator(val left: Formula, val right: Formula): Formula()

class Proposition(val propString: String): Formula() {

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
        val indishStates = getIndishStates(agent, state) //TODO Fix not using updated model
        if (indishStates.all { inner.check(it, model) }){
            return true
        }
        return false
    }
}

//TODO Look into optimizing by reusing the same updated model when checking multiple states
class Announcement(val announcement: Formula, val inner: Formula): BinaryOperator(announcement, inner){
    override fun check(state: State, model: Model): Boolean {
        if (!announcement.check(state, model)) {
            return true
        }
        else {
            val updModel = updateModel(announcement, model)
            return inner.check(state, updModel)
        }
    }
}

//TODO Look into optimizing by caching as well
class GroupAnn(val agents: List<AgentItem>, val inner: Formula): Formula() {
    override fun check(state: State, model: Model): Boolean {
        //Formula is already true, agents simply announce Top
        if (inner.check(state, model)) {
            println("Already true")
            return true
        }

        //Atomic permanence and empty group is powerless
        if (agents.isEmpty() || !containsKnowsOp(inner)) {
            println("Atomic permanence or empty group")
            return inner.check(state, model)
        }

        //Is it really enough to only look at props contained in the formula?
        val pooledModel = poolGroupKnowledge(agents, model)
        val extractProps = extractProps(inner)
        val knownProps = extractProps.filter {
            Knows(agents.first(),Proposition(it)).check(state, pooledModel) //TODO Fix
        }

        //Update model by simulating successive announcements
        val updatedModel = knownProps.fold(model) { acc, prop -> updateModel(Proposition(prop), acc) }
        return inner.check(state, updatedModel)
    }

}