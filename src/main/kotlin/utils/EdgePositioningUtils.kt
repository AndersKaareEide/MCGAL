package utils

import javafx.beans.binding.DoubleBinding
import javafx.beans.binding.ObjectBinding
import javafx.scene.control.Label
import tornadofx.*


fun Label.getPositionBinding(x1: DoubleBinding, x2: DoubleBinding, y1: DoubleBinding, y2: DoubleBinding)
        : ObjectBinding<LabelPos?> {
    return objectBinding(x1, x2, y1, y2, widthProperty(), heightProperty()) {

        val startPosVec = Pair(x1.value, y1.value)

        val lineVector = Pair(x2.value - x1.value, y2.value - y1.value)
        val lineLength = Math.sqrt(Math.pow(x2.value - x1.value, 2.0) + Math.pow(y2.value - y1.value, 2.0))

        val vectorU = lineVector / lineLength

        var eta = Pair(vectorU.second, -vectorU.first)
        var etaAngle = Math.toDegrees(Math.atan2(eta.second, eta.first)) + 90

        var widthOffset = widthProperty().value / 2

        if (etaAngle > 90 && etaAngle < 270) {
            etaAngle -= 180
            eta *= -1.0
            widthOffset *= -1.0
        }

        val labelPosition = startPosVec + vectorU * (lineLength / 2 - widthOffset) + eta * (heightProperty().value + 10)
        LabelPos(labelPosition.first, labelPosition.second, etaAngle)
    }
}

data class LabelPos (val xPos: Double, val yPos: Double, val rotDegrees: Double)

//TODO Move out
private operator fun Pair<Double, Double>.plus(other: Pair<Double, Double>): Pair<Double, Double> {
    return Pair(first + other.first, second + other.second)
}

private operator fun Pair<Double, Double>.minus(other: Pair<Double, Double>): Pair<Double, Double> {
    return Pair(first - other.first, second - other.second)
}

private operator fun Pair<Double, Double>.times(other: Pair<Double, Double>): Pair<Double, Double> {
    return Pair(first * other.first, second * other.second)
}

private operator fun Pair<Double, Double>.div(other: Pair<Double, Double>): Pair<Double, Double> {
    return Pair(first / other.first, second / other.second)
}

private operator fun Pair<Double, Double>.times(other: Double): Pair<Double, Double>{
    return this * Pair(other, other)
}

private operator fun Pair<Double, Double>.div(other: Double): Pair<Double, Double>{
    return this / Pair(other, other)
}
