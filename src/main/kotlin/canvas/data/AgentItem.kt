package canvas.data

import javafx.beans.property.ObjectProperty
import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.SimpleStringProperty
import tornadofx.*
import java.io.Serializable

class AgentItem(name: String, isSelected: Boolean): Comparable<AgentItem>, Serializable {

    val nameProperty = SimpleStringProperty(this, "name", name)
    var name by nameProperty

    val isSelectedProperty = SimpleBooleanProperty(this, "isSelected", isSelected)
    var isSelected by isSelectedProperty

    //TODO Flesh out to make a2 < a12
    override fun compareTo(other: AgentItem): Int {
        return name.compareTo(other.name)
    }

    override fun equals(other: Any?): Boolean {
        return other is AgentItem && other.name == name
    }

    override fun hashCode(): Int {
        var result = nameProperty.hashCode()
        result = 31 * result + isSelectedProperty.hashCode()
        return result
    }

}

class AgentItemModel(property: ObjectProperty<AgentItem>):
        ItemViewModel<AgentItem>(itemProperty = property), Comparable<AgentItemModel>{

    override fun compareTo(other: AgentItemModel): Int {
        return item.compareTo(other.item)
    }

    val nameText = bind(autocommit = true){
        item?.nameProperty
    }

    val isSelected = bind(autocommit = true){
        item?.isSelectedProperty
    }
}
