package io

import java.io.Serializable

data class SerializableModel(val states: List<SerializableState>, val edges: List<SerializableEdge>,
                             val agents: List<SerializableAgent>, val props: List<SerializableProp>) : Serializable

data class SerializableState(val name: String, val xPos: Double, val yPos: Double, val props: List<SerializableProp>): Serializable

data class SerializableEdge(val parent1: SerializableState, val parent2: SerializableState,
                            val id: String, val agents: List<SerializableAgent>): Serializable

data class SerializableAgent(val name: String, val isSelected: Boolean): Serializable

data class SerializableProp(val propString: String, val isSelected: Boolean): Serializable