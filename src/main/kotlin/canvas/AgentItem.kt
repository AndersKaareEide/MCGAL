package canvas

import javafx.beans.property.ObjectProperty
import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.SimpleStringProperty
import tornadofx.*

class AgentItem(name: String, isSelected: Boolean): Comparable<AgentItem> {

    //TODO Flesh out to make a2 < a12
    override fun compareTo(other: AgentItem): Int {
        return name.compareTo(other.name)
    }

    override fun equals(other: Any?): Boolean {
        if (other is AgentItem && other.name == name) {
            return true
        }
        return false
    }

    override fun hashCode(): Int {
        var result = nameProperty.hashCode()
        result = 31 * result + isSelectedProperty.hashCode()
        return result
    }

    val nameProperty = SimpleStringProperty(name)
    var name by nameProperty

    val isSelectedProperty = SimpleBooleanProperty(isSelected)
    var isSelected by isSelectedProperty
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
