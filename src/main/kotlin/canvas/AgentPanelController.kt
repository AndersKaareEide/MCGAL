package canvas

import javafx.collections.FXCollections
import javafx.collections.ObservableList
import tornadofx.*

class AgentPanelController: Controller() {


    val agents = SortedFilteredList<AgentItem>()

    init {
        //TODO Remove, used for manual testing
        agents.addAll(AgentItem("a0",true), AgentItem("a1",false),
                      AgentItem("b",true), AgentItem("c",true))
    }

    fun getSelected(): ObservableList<AgentItem>{
        //TODO Find out whether filteredList returns a new list or reuses the same
        return agents.filtered { it.isSelected }!!
    }

    fun removeAgent(agent: AgentItemModel){
        agents.remove(agent.item)

    }

    fun addAgent(agentName: String){
        if (agentName != "") {
            agents.add(AgentItem(agentName, true))
        }
    }
}