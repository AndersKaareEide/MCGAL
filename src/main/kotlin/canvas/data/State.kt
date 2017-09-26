package canvas.data

import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.SimpleDoubleProperty
import javafx.beans.property.SimpleListProperty
import javafx.beans.property.SimpleStringProperty
import javafx.collections.FXCollections
import javafx.collections.ObservableList
import sidepanels.propertypanel.PropositionItem
import tornadofx.*
import java.io.Serializable

class State(name: String, xPos: Double, yPos: Double, props: List<PropositionItem> = mutableListOf()): Serializable {

    val nameProperty = SimpleStringProperty(this, "name", name)
    var name by nameProperty

    val xProperty = SimpleDoubleProperty(this, "xPos", xPos)
    var xPos by xProperty

    val yProperty = SimpleDoubleProperty(this, "yPos", yPos)
    var yPos by yProperty

    val inEdgesProperty = SimpleListProperty<Edge>(this, "inEdges", FXCollections.observableArrayList())
    var inEdges by inEdgesProperty

    val outEdgesProperty = SimpleListProperty<Edge>(this, "outEdges", FXCollections.observableArrayList())
    var outEdges by outEdgesProperty

    val propsProperty = SimpleListProperty<PropositionItem>(this, "agents", props.observable())
    var props by propsProperty

    val hiddenProperty = SimpleBooleanProperty(this, "isHidden", false)
    var isHidden by hiddenProperty

    val selectedProperty = SimpleBooleanProperty(this, "isSelected", false)
    var isSelected by selectedProperty
}

