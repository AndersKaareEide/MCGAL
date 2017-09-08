import javafx.beans.property.SimpleStringProperty
import tornadofx.*

class Edge(val parent1: State, val parent2: State) {

    val id: String by property { SimpleStringProperty(this, "id", ) }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other is Edge && parent1.equals(other.parent2)) return true //Edges have same parents
        return false
    }

    override fun hashCode(): Int {
        var result = parent1.hashCode()
        result = 31 * result + parent2.hashCode()
        result = 31 * result + id.hashCode()
        return result
    }
}

class EdgeModel : ItemViewModel<Edge>() {
    val parent1 = bind(Edge::parent1)
    val parent2 = bind(Edge::parent2)
    val id = bind(Edge::id)
}
