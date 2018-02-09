package canvas.data

import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.SimpleListProperty
import tornadofx.*
import java.io.Serializable

class Edge private constructor(val outParent: State, val inParent: State, agents: List<AgentItem>)
    : Serializable, ModelComponent {
    companion object {
        fun edgeBetween(inParent: State, outParent: State, agents: List<AgentItem>): Edge {
            val edge = Edge(inParent, outParent, agents)

            inParent.inEdges.add(edge)
            outParent.outEdges.add(edge)
            edge.hiddenProperty.bind(inParent.hiddenProperty.or(outParent.hiddenProperty))

            return edge
        }
    }

    var id: String = outParent.name + inParent.name

    val agentsProperty = SimpleListProperty<AgentItem>(this, "agents", agents.observable())
    var agents by agentsProperty

    val hiddenProperty = SimpleBooleanProperty(this, "hidden", false)
    var hidden by hiddenProperty

    val selectedProperty = SimpleBooleanProperty(this, "isSelected", false)
    override var isSelected by selectedProperty

    override fun equals(other: Any?): Boolean {
        if (this === other)
            return true
        //Edges have same parents
        if (other is Edge &&
                ((outParent == other.inParent && inParent == other.outParent)
             || (outParent == other.outParent) && inParent == other.inParent)) {
            return true
        }
        return false
    }

    override fun hashCode(): Int {
        var result = outParent.hashCode()
        result = 31 * result + inParent.hashCode()
        result = 31 * result + id.hashCode()
        return result
    }
}