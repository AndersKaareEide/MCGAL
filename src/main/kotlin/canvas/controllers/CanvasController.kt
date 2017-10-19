package canvas.controllers

import canvas.data.Edge
import canvas.data.State
import canvas.data.Model
import canvas.data.ModelComponent
import io.ModelSerializer
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

    var isDragging = false


    var clipBoardModel: Model? = null

    val model = Model(stateController.states, edgeController.edges,
            agentController.agents.items, propController.propositions)

    fun handleCanvasClick(event: MouseEvent) {
        if (clickMode == ClickMode.STATES) {
            stateController.addState(event)
        } else if (!event.isShiftDown){
            clearSelectedComponents()
        }
    }

    fun handleSelectionMPress(event: MouseEvent, item: ModelComponent){
        if (item.isSelected){
            return //Wait until the click listener fires so that the user can drag multiple items without holding shift
        } else {
            handleSelectionClick(event, item)
        }

    }

    fun handleSelectionClick(event: MouseEvent, item: ModelComponent){
        if (isDragging){
            isDragging = false
            return //The user dragged something, maintain selection
        }

        if (!event.isShiftDown) {
            clearSelectedComponents()
        }
        selectItem(item)
    }

    fun selectItem(item: ModelComponent) {
        item.isSelected = true

        when (item) {
            is State -> stateController.selectState(item)
            is Edge -> edgeController.selectEdge(item)
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
        propController.propositions.clear()
        propController.propositions.addAll(model.props)
    }

    fun importModel(model: Model) {
        clearSelectedComponents()
        agentController.agents.addAll(model.agents.filter { !agentController.agents.contains(it) })
        propController.propositions.addAll(model.props.filter { !propController.propositions.contains(it) })

        model.states.forEach {
            it.name = stateController.getNextStateID()
            it.isSelected = true
            stateController.states.add(it)
            stateController.selectState(it)
        }
        model.edges.forEach {
            it.id = it.parent1.name + it.parent2.name
            edgeController.edges.add(it)
        }
    }

    fun clearModel() {
        stateController.states.clear()
        edgeController.edges.clear()
        agentController.agents.clear()
        propController.propositions.clear()
    }

    fun selectStates(bounds: Bounds, it: MouseDragEvent) {
        if (!it.isShiftDown) {
            clearSelectedComponents()
        }
        stateController.selectFromBounds(bounds)

        /*TODO Select edges from bounds as well via position of label? Hard to find coordinates of label without
          TODO introducing extra overhead */
    }

    fun clearSelectedComponents() {
        (stateController.selectedStates + edgeController.selectedEdges).forEach {
            it as ModelComponent
            it.isSelected = false
        }
        stateController.clearSelected()
        edgeController.clearSelected()
    }

    fun copySelection() {
        val selectedStates = stateController.selectedStates.toList()
        clipBoardModel = Model(
                selectedStates,
                edgeController.edges.filter { selectedStates.contains(it.parent1) && selectedStates.contains(it.parent2) },
                agentController.agents,
                propController.propositions
        )
    }

    //TODO Center pasted components on mouse cursor by modifying the model first?
    fun pasteComponents() {
        if (clipBoardModel != null){
            clipBoardModel = ModelSerializer.createCopy(clipBoardModel!!) //Clone anew each time paste is called
            importModel(clipBoardModel!!)
        }
    }

}
