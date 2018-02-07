package sidepanels.propertypanel

import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.SimpleStringProperty
import tornadofx.*

class PropositionItem(propString: String, isSelected: Boolean = false): Comparable<PropositionItem>{

    val propStringProperty = SimpleStringProperty(this, "proposition", propString)
    var propString: String by propStringProperty

    val isSelectedProperty = SimpleBooleanProperty(this, "isSelected", isSelected)
    var isSelected by isSelectedProperty

    override fun compareTo(other: PropositionItem): Int {
        return propString.compareTo(other.propString)
    }

    override fun equals(other: Any?): Boolean {
        return other is PropositionItem && propString == other.propString
    }

    override fun hashCode(): Int {
        var result = propStringProperty.hashCode()
        result = 31 * result + isSelectedProperty.hashCode()
        return result
    }


}