package canvas

import javafx.collections.ObservableList

class Model(val states: ObservableList<State>,
            val edges: ObservableList<Edge>,
            val agents: ObservableList<AgentItem>)