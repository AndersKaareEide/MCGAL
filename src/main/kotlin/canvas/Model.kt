package canvas

import javafx.collections.ObservableList

class Model(val states: List<State>,
            val edges: List<Edge>,
            val agents: List<AgentItem>)