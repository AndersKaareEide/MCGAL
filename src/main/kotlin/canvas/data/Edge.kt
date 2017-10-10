package canvas.data

import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.SimpleListProperty
import tornadofx.*
import java.io.Serializable

class Edge(val parent1: State, val parent2: State, agents: List<AgentItem>) : Serializable, ModelComponent {

    var id: String = parent1.name + parent2.name

    val agentsProperty = SimpleListProperty<AgentItem>(this, "agents", agents.observable())
    var agents by agentsProperty

    val hiddenProperty = SimpleBooleanProperty(this, "hidden", false)
    var hidden by hiddenProperty

    val selectedProperty = SimpleBooleanProperty(this, "isSelected", false)
    override var isSelected by selectedProperty

    init {
        parent1.inEdges.add(this)
        parent2.outEdges.add(this)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other)
            return true
        //Edges have same parents
        if (other is Edge &&
                ((parent1 == other.parent2 && parent2 == other.parent1)
             || (parent1 == other.parent1) && parent2 == other.parent2)) {
            return true
        }
        return false
    }

    override fun hashCode(): Int {
        var result = parent1.hashCode()
        result = 31 * result + parent2.hashCode()
        result = 31 * result + id.hashCode()
        return result
    }
}

