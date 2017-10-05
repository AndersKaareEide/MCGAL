package sidepanels.propertypanel

import javafx.geometry.Pos
import javafx.scene.layout.Priority
import tornadofx.*

class PropListFragment() : ListCellFragment<PropositionItem>() {

    val controller: PropPanelController by inject()
    val proposition = PropItemModel(itemProperty)

    override val root = hbox {
        prefHeight = 20.0
        alignment = Pos.CENTER

        val checkbox = checkbox(property = proposition.isSelected) {
            action {
                startEdit()
                commitEdit(item)
            }
        }

        label(proposition.propText){
            hgrow = Priority.ALWAYS
            useMaxSize = true
        }

        button("Remove") {
            removeWhen { parent.hoverProperty().not() }
            action { controller.removeProposition(proposition.item) }
        }

        setOnMouseClicked { checkbox.fire() }
    }
}