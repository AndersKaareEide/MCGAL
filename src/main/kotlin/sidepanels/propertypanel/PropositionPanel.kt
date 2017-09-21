package sidepanels.propertypanel

import javafx.event.ActionEvent
import javafx.scene.layout.Priority
import sidepanels.agentpanel.AgentListFragment
import tornadofx.*

class PropositionPanel : View("My View") {

    val controller: PropPanelController by inject()

    override val root = vbox {
        hbox {
            val propField = textfield {
                promptText = "Enter propositions here"
                hgrow = Priority.ALWAYS
                action {
                    controller.addProposition(text)
                    clear()
                }
            }
            button {
                text = "_Add"
                action { propField.fireEvent(ActionEvent()) }
            }
        }

        listview(controller.propositions.sorted()) {
            cellFragment(PropListFragment::class)
            vgrow = Priority.ALWAYS
        }

    }
}

