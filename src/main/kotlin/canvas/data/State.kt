package canvas.data

import canvas.views.Draggable
import javafx.beans.property.*
import javafx.collections.FXCollections
import javafx.collections.ObservableList
import sidepanels.debugpanel.DebugLabelItem
import sidepanels.propertypanel.PropositionItem
import tornadofx.*
import java.io.Serializable

class State(name: String, xPos: Double = 0.0, yPos: Double = 0.0,
            props: List<PropositionItem> = mutableListOf()): Serializable, ModelComponent, Draggable {

    val nameProperty = SimpleStringProperty(this, "name", name)
    var name: String by nameProperty

    val edgesProperty = SimpleListProperty<Edge>(this, "edges", FXCollections.observableArrayList())
    var edges: ObservableList<Edge> by edgesProperty

    override val xProperty = SimpleDoubleProperty(this, "xPos", xPos)
    override var xPos by xProperty

    override val yProperty = SimpleDoubleProperty(this, "yPos", yPos)
    override var yPos by yProperty

    val propsProperty = SimpleListProperty<PropositionItem>(this, "props", props.observable())
    var props: ObservableList<PropositionItem> by propsProperty

    val validationStyleProp = SimpleObjectProperty<CssRule>(this, "validationStyle", null)
    var validationStyle: CssRule? by validationStyleProp

    val selectedProperty = SimpleBooleanProperty(this, "isSelected", false)
    override var isSelected by selectedProperty

    val debugLabelsProperty = SimpleListProperty<ObservableList<DebugLabelItem>>(this, "debugLabels", FXCollections.observableArrayList())
    var debugLabels: ObservableList<ObservableList<DebugLabelItem>> by debugLabelsProperty

    val hiddenProperty = SimpleBooleanProperty(this, "isHidden", false)
    var isHidden by hiddenProperty

    override fun toString(): String = name
}

