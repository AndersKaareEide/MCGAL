package canvas.controllers

import javafx.scene.input.KeyCode
import javafx.scene.input.KeyCodeCombination
import javafx.scene.input.KeyCombination

enum class ClickMode(val title: String, val accelerator: KeyCodeCombination) {
    MOVING("Move elements", KeyCodeCombination(KeyCode.Z, KeyCombination.ALT_DOWN)),
    STATES("Draw states", KeyCodeCombination(KeyCode.S, KeyCombination.ALT_DOWN)),
    LINES("Draw lines", KeyCodeCombination(KeyCode.X, KeyCombination.ALT_DOWN))
}