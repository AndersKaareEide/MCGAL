package canvas

import javafx.event.ActionEvent
import javafx.scene.layout.Priority
import tornadofx.*

class AgentPanel : View() {

    val controller: AgentPanelController by inject()

    override val root = vbox {
        prefWidth = 200.0

        hbox {
            val agentField = textfield {
                promptText = "Agent name here"
                hgrow = Priority.ALWAYS
                action {
                    controller.addAgent(text)
                    clear()
                }
            }
            button {
                text = "_Add"
                action { agentField.fireEvent(ActionEvent()) }
            }
        }

        listview(controller.agents) {
            cellFragment(AgentListFragment::class)
            vgrow = Priority.ALWAYS
        }
    }
}