package canvas.data

import sidepanels.propertypanel.PropositionItem

data class Model(val states: List<State>,
                 val edges: List<Edge>,
                 val agents: List<AgentItem>,
                 val props: List<PropositionItem>)

