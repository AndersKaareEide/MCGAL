package sidepanels.propertypanel

import javafx.scene.layout.Priority
import tornadofx.*

class PropListFragment() : ListCellFragment<PropositionItem>() {

    val controller: PropPanelController by inject()
    val proposition = PropItemModel(itemProperty)

    override val root = hbox {
        checkbox(property = proposition.isSelected) {
            action {
                startEdit()
                commitEdit(item)
            }
        }

        label(proposition.propText){
            hgrow = Priority.ALWAYS
            useMaxSize = true
        }

    }
}