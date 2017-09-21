package sidepanels.agentpanel

import canvas.data.AgentItem
import canvas.data.AgentItemModel
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
