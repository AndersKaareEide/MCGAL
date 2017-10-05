package sidepanels.propertypanel

import canvas.controllers.StateController
import formulaParser.PropertyNotFoundException
import javafx.collections.ObservableList
import tornadofx.*
import utils.defaultProps

//TODO Make generic superclass to reduce duplication with AgentPanelController
class PropPanelController : Controller() {

    val stateController: StateController by inject()
    val propositions = SortedFilteredList<PropositionItem>()

    init {
        propositions.addAll(defaultProps)
    }

    fun getSelected() : ObservableList<PropositionItem> {
        return propositions.filtered { it.isSelected }!!
    }

    fun getProposition(propString: String) : PropositionItem {
        for (proposition in propositions){
            if (proposition.propString == propString){
                return proposition
            }
        }
        throw PropertyNotFoundException(propString)
    }

    fun addProposition(propString: String) {
        if (propString != "") {
            val newProp = PropositionItem(propString, true)
            if (propositions.contains(newProp)) {
                var propNumber = 0
                newProp.propString = propString + propNumber
                while (propositions.contains(newProp)) {
                    propNumber++
                    newProp.propString = propString + propNumber
                }
            }
            propositions.add(newProp)
        }
    }

    fun removeProposition(proposition: PropositionItem) {
        propositions.remove(proposition)
        stateController.states.forEach { it.props.remove(proposition) }
    }

    //TODO Add function for 'duplicating' the graph when adding new props
}
