package sidepanels.agentpanel

import javafx.event.ActionEvent
import javafx.scene.layout.Priority
import tornadofx.*

class AgentPanel : View() {

    val controller: AgentPanelController by inject()

    override val root = vbox {

        hbox {
            val agentField = textfield {
                promptText = "Agent name here"
                hgrow = Priority.ALWAYS
                action {
                    controller.addAgent(text.capitalize())
                    clear()
                }
            }
            button {
                text = "_Add"
                action { agentField.fireEvent(ActionEvent()) }
            }
        }

        listview(controller.agents.sorted()) {
            cellFragment(AgentListFragment::class)
            vgrow = Priority.ALWAYS
        }
    }
}