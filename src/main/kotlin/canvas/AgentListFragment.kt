package canvas

import javafx.beans.property.ObjectProperty
import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.SimpleStringProperty
import javafx.scene.layout.Priority
import tornadofx.*

class AgentListFragment: ListCellFragment<AgentItem>() {
    val agent = AgentItemModel(itemProperty)
    val controller: AgentPanelController by inject()

    override val root = hbox {

        checkbox(property = agent.isSelected) {
            action {
                startEdit()
                commitEdit(item)
            }
        }

        label(agent.nameText){
            hgrow = Priority.ALWAYS
            useMaxSize = true
        }

        button("Remove") {
            removeWhen { parent.hoverProperty().not() }
            action { controller.removeAgent(agent) }
        }
    }
}

class AgentItem(name: String, isSelected: Boolean): Comparable<AgentItem> {

    //TODO Flesh out to make a2 < a12
    override fun compareTo(other: AgentItem): Int {
        return name.compareTo(other.name)
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
