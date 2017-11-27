package canvas.data

import canvas.views.Draggable
import javafx.beans.property.*
import javafx.collections.FXCollections
import javafx.collections.ObservableList
import sidepanels.debugpanel.DebugLabelItem
import sidepanels.propertypanel.PropositionItem
import tornadofx.*
import java.io.Serializable

class State(name: String, xPos: Double, yPos: Double, props: List<PropositionItem> = mutableListOf()): Serializable, ModelComponent, Draggable {

    val nameProperty = SimpleStringProperty(this, "name", name)
    var name by nameProperty

    override val xProperty = SimpleDoubleProperty(this, "xPos", xPos)
    override var xPos by xProperty

    override val yProperty = SimpleDoubleProperty(this, "yPos", yPos)
    override var yPos by yProperty

    val inEdgesProperty = SimpleListProperty<Edge>(this, "inEdges", FXCollections.observableArrayList())
    var inEdges by inEdgesProperty

    val outEdgesProperty = SimpleListProperty<Edge>(this, "outEdges", FXCollections.observableArrayList())
    var outEdges by outEdgesProperty

    val propsProperty = SimpleListProperty<PropositionItem>(this, "props", props.observable())
    var props by propsProperty

    val validationStyleProp = SimpleObjectProperty<CssRule>(this, "validationStyle", null)
    var validationStyle by validationStyleProp

    val selectedProperty = SimpleBooleanProperty(this, "isSelected", false)
    override var isSelected by selectedProperty

    val debugLabelsProperty = SimpleListProperty<ObservableList<DebugLabelItem>>(this, "debugLabels", FXCollections.observableArrayList())
    var debugLabels by debugLabelsProperty

    val hiddenProperty = SimpleBooleanProperty(this, "isHidden", false)
    var isHidden by hiddenProperty
}

