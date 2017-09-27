package canvas.controllers

import canvas.data.Model
import javafx.beans.property.SimpleObjectProperty
import sidepanels.agentpanel.AgentPanelController
import sidepanels.propertypanel.PropPanelController
import tornadofx.*

class CanvasController : Controller() {

    val stateController: StateController by inject()
    val edgeController: EdgeController by inject()
    val agentController: AgentPanelController by inject()
    val propController: PropPanelController by inject()

    val model = Model(stateController.states, edgeController.edges,
            agentController.agents.items, propController.propositions)

    fun loadModel(model: Model) {
        stateController.states.setAll(model.states)
        edgeController.edges.setAll(model.edges)
        agentController.agents.clear()
        agentController.agents.addAll(model.agents)
    }

    fun clearModel() {
        stateController.states.clear()
        edgeController.edges.clear()
        agentController.agents.clear()
        propController.propositions.clear()
    }
}