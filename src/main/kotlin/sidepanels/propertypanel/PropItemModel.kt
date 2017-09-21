package sidepanels.propertypanel

import javafx.beans.property.ObjectProperty
import tornadofx.*

class PropItemModel(property: ObjectProperty<PropositionItem>) :
        ItemViewModel<PropositionItem>(itemProperty = property), Comparable<PropItemModel> {

    val propText = bind(autocommit = true) { item?.propStringProperty }
    val isSelected = bind(autocommit = true) { item?.isSelectedProperty }

    override fun compareTo(other: PropItemModel): Int {
        return item.compareTo(other.item)
    }
}