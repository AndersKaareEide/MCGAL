package io

import canvas.data.AgentItem
import canvas.data.Edge.Companion.edgeBetween
import canvas.data.Model
import canvas.data.State
import sidepanels.propertypanel.PropositionItem
import java.io.*


object ModelSerializer {

    fun serializeModel(model: Model, file: File){
        val serializableModel = makeSerializable(model)

        val fileOutput = FileOutputStream(file)
        val outStream = ObjectOutputStream(fileOutput)

        outStream.writeObject(serializableModel)
    }

    fun deserializeModel(file: File) : Model {
        val fileInput = FileInputStream(file)
        val inStream = ObjectInputStream(fileInput)

        val deserializedModel = inStream.readObject() as SerializableModel
        return convertSerializableModel(deserializedModel)
    }

    fun createCopy(input: Model): Model {
        return convertSerializableModel(makeSerializable(input))
    }

    private fun makeSerializable(model: Model): SerializableModel {
        val serializableProps = model.props.map {
            SerializableProp(it.propString, it.isSelected)
        }.associateBy{ it.propString }

        val serializableAgents = model.agents.map {
            SerializableAgent(it.name, it.isSelected)
        }.associateBy { it.name }

        val serializableStates = model.states.map {
            SerializableState(it.name, it.xPos, it.yPos, it.props.map { serializableProps.get(it.propString)!! })
        }.associateBy { it.name }

        val serializableEdges = model.edges.map {
            SerializableEdge(serializableStates[it.outParent.name]!!, serializableStates[it.inParent.name]!!,
                             it.id, it.agents.map { serializableAgents[it.name]!! })
        }

        return SerializableModel(serializableStates.values.toList(), serializableEdges,
                                 serializableAgents.values.toList(), serializableProps.values.toList())
    }

    private fun convertSerializableModel(deserializedModel: SerializableModel): Model {
        val props = deserializedModel.props.map {
            PropositionItem(it.propString, it.isSelected)
        }.associateBy { it.propString }

        val agents = deserializedModel.agents.map {
            AgentItem(it.name, it.isSelected)
        }.associateBy { it.name }

        val states = deserializedModel.states.map {
            State(it.name, it.xPos, it.yPos, it.props.map { props[it.propString]!! })
        }.associateBy { it.name }

        val edges = deserializedModel.edges.map {
            edgeBetween(states[it.parent1.name]!!, states[it.parent2.name]!!, it.agents.map { agents[it.name]!! })
        }.toMutableList()

        return Model(states.values.toMutableList(), edges, agents.values.toMutableList(), props.values.toMutableList())
    }
}