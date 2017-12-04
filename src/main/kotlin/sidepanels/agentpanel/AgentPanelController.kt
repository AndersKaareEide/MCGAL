package sidepanels.agentpanel
import canvas.controllers.EdgeController
import canvas.data.AgentItem
import canvas.data.AgentItemModel
import formulaParser.AgentNotFoundException
import javafx.collections.ObservableList
import tornadofx.*
import utils.defaultAgents

class AgentPanelController: Controller() {

    val edgeController: EdgeController by inject()
    val agents = SortedFilteredList<AgentItem>()

    init {
        agents.addAll(defaultAgents)
    }

    fun getSelected(): ObservableList<AgentItem> {
        return agents.filtered { it.isSelected }!!
    }

    fun deselectAll(){
        agents.forEach { it.isSelected = false }
    }

    fun removeAgent(agent: AgentItemModel){
        edgeController.removeAgent(agent.item)
        agents.remove(agent.item)
    }

    fun addAgent(agentName: String){
        if (agentName != "") {
            val newAgent = AgentItem(agentName, true)
            if (agents.contains(newAgent)) {
                var agentNumber = 0
                newAgent.name = agentName + agentNumber
                while (agents.contains(newAgent)) {
                    agentNumber++
                    newAgent.name = agentName + agentNumber
                }
            }
            agents.add(newAgent)
        }
    }

    /**
     * Method used to retrieve AgentItems by name from the list of agents
     */
    fun getAgent(agentName: String): AgentItem {
        for (agent in agents){
            if (agent.name == agentName){
                return agent
            }
        }
        throw AgentNotFoundException(agentName)
    }
}