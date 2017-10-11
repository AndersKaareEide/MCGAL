package sidepanels.agentpanel

import canvas.data.AgentItem
import canvas.data.AgentItemModel
import javafx.geometry.Pos
import javafx.scene.layout.Priority
import tornadofx.*

class AgentListFragment: ListCellFragment<AgentItem>() {

    val agent = AgentItemModel(itemProperty)
    val controller: AgentPanelController by inject()

    override val root = hbox {
        prefHeight = 20.0
        alignment = Pos.CENTER

        val checkbox = checkbox(property = agent.isSelected) {
            setOnMouseClicked {
                if (it.isControlDown){
                    controller.deselectAll()
                }
                this.fire()
            }
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

        setOnMouseClicked {
            if (it.isControlDown){
                controller.deselectAll()
            }
            checkbox.fire()
        }
    }
}
