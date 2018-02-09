package canvas.controllers

import canvas.data.Edge
import canvas.data.Model
import canvas.data.ModelComponent
import canvas.data.State
import canvas.views.Canvas
import formulaParser.formulaDebugger.Debugger
import formulafield.FormulaFieldController
import io.ModelSerializer
import javafx.beans.property.SimpleObjectProperty
import javafx.geometry.Bounds
import javafx.scene.control.Tab
import javafx.scene.input.MouseDragEvent
import javafx.scene.input.MouseEvent
import sidepanels.agentpanel.AgentPanelController
import sidepanels.debugpanel.DebugController
import sidepanels.debugpanel.DebugPanel
import sidepanels.propertypanel.PropPanelController
import tornadofx.*

@Suppress("UNCHECKED_CAST")
class CanvasController : Controller() {
    val formulaController: FormulaFieldController by inject()
    val stateController: StateController by inject()
    val edgeController: EdgeController by inject()
    val agentController: AgentPanelController by inject()
    val propController: PropPanelController by inject()
    val debugController: DebugController by inject()

    private val debugPanel: DebugPanel by inject()
    private val canvas: Canvas by inject()

    private var debugTab: Tab? = null

    val clickModeProperty = SimpleObjectProperty<ClickMode>(this, "clickMode", ClickMode.MOVING)
    var clickMode by clickModeProperty

    var isDragging = false

    var clipBoardModel: Model? = null
    var stateSelectionCallback: ((State) -> Unit)? = null //Ugly hack used to start the debugger


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
            is State -> {
                stateController.selectState(item)
                stateSelectionCallback?.invoke(item)
            }
            is Edge -> edgeController.selectEdge(item)
        }
    }

    fun removeSelected(){
        stateController.removeSelected()
        edgeController.removeSelected()
    }

    fun loadModel(model: Model) {
        formulaController.clearLabels()
        stateController.states.setAll(model.states)
        edgeController.edges.setAll(model.edges)
        agentController.agents.clear()
        agentController.agents.addAll(model.agents)
        propController.propositions.clear()
        propController.propositions.addAll(model.props)
    }

    fun importModel(model: Model) {
        clearSelectedComponents()
        formulaController.clearLabels()
        agentController.agents.addAll(model.agents.filter { !agentController.agents.contains(it) })
        propController.propositions.addAll(model.props.filter { !propController.propositions.contains(it) })

        model.states.forEach {
            it.name = stateController.getNextStateID()
            it.isSelected = true
            stateController.states.add(it)
            stateController.selectState(it)
        }
        model.edges.forEach {
            it.id = it.outParent.name + it.inParent.name
            edgeController.edges.add(it)
        }
    }

    fun clearModel() {
        formulaController.clearLabels()
        stateController.states.clear()
        edgeController.edges.clear()
        agentController.agents.clear()
        propController.propositions.clear()
        debugController.clearDebugger()
        Debugger.clear()
    }

    fun selectStates(bounds: Bounds, it: MouseDragEvent) {
        if (!it.isShiftDown) {
            clearSelectedComponents()
        }
        stateController.selectFromBounds(bounds)
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
                edgeController.edges.filter { selectedStates.contains(it.outParent) && selectedStates.contains(it.inParent) },
                agentController.agents,
                propController.propositions
        )
    }

    fun pasteComponents() {
        if (clipBoardModel != null){
            clipBoardModel = ModelSerializer.createCopy(clipBoardModel!!) //Clone anew each time paste is called
            importModel(clipBoardModel!!)
        }
    }

    fun showDebugPanelTab(){
        if (debugTab == null) {
            debugTab = Tab("Debugger", debugPanel.root)
        }
        if (!canvas.sidePanel.tabs.contains(debugTab)) {
            canvas.sidePanel.tabs.add(debugTab)
        }
        canvas.sidePanel.selectionModel.select(2)
    }

    fun hideDebugPanelTab(){
        canvas.sidePanel.tabs.remove(debugTab)
    }

}
