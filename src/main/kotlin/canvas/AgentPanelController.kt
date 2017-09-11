package canvas

import javafx.collections.FXCollections
import tornadofx.*

class AgentPanelController: Controller() {

    val agents = FXCollections.observableArrayList(
            AgentItem("a0",true), AgentItem("a1",false),
            AgentItem("b",true), AgentItem("c",true))


    fun removeAgent(agent: AgentItemModel){
        agents.remove(agent.item)
    }

    fun addAgent(agentName: String){
        if (agentName != "") {
            agents.add(AgentItem(agentName, true))
        }
    }
}