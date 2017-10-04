package canvas.controllers

import canvas.data.Edge
import canvas.data.State
import canvas.data.Model
import canvas.data.ModelComponent
import javafx.beans.property.SimpleObjectProperty
import javafx.geometry.Bounds
import javafx.scene.input.MouseDragEvent
import javafx.scene.input.MouseEvent
import sidepanels.agentpanel.AgentPanelController
import sidepanels.propertypanel.PropPanelController
import tornadofx.*

@Suppress("UNCHECKED_CAST")
class CanvasController : Controller() {

    val stateController: StateController by inject()
    val edgeController: EdgeController by inject()
    val agentController: AgentPanelController by inject()
    val propController: PropPanelController by inject()

    val clickModeProperty = SimpleObjectProperty<ClickMode>(this, "clickMode", ClickMode.MOVING)
    var clickMode by clickModeProperty

    val model = Model(stateController.states, edgeController.edges,
            agentController.agents.items, propController.propositions)

    fun handleSelectionClick(it: MouseEvent, item: ModelComponent){
        clearSelectedComponents(it)
        item.isSelected = true

        when(item){
            is State -> stateController.selectState(item)
            is Edge ->  edgeController.selectEdge(item)
        }
    }

    fun removeSelected(){
        stateController.removeSelected()
        edgeController.removeSelected()
    }

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

    fun selectStates(bounds: Bounds, it: MouseDragEvent) {
        clearSelectedComponents(it)
        stateController.selectFromBounds(bounds)

        /*TODO Select edges from bounds as well via position of label? Hard to find coordinates of label without
          TODO introducing extra overhead */
    }

    fun clearSelectedComponents(it: MouseEvent) {
        if (!it.isShiftDown) {
            (stateController.selectedStates + edgeController.selectedEdges).forEach {
                it as ModelComponent
                it.isSelected = false
            }
            stateController.clearSelected()
            edgeController.clearSelected()
        }
    }
}
