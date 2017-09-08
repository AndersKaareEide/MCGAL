package dickingaround

import javafx.collections.FXCollections
import javafx.scene.control.ProgressBar
import javafx.scene.control.TextField
import tornadofx.*
import tornadofx.Controller

class TestView : View("My View") {

    val buttonController: TestController by inject()
    val listController: ListController by inject()
    var inputField: TextField by singleAssign()
    var progressBar: ProgressBar by singleAssign()

    override val root = vbox {
        progressBar = ProgressBar()
        progressBar.hide()

        label("Input")
        inputField = textfield()
        button("commit") {
            action {
                runAsync {
                    buttonController.doLogging(inputField.text)
                } ui { inputField.clear() }
            }
        }

        listview(listController.values)

    }

    class ListController: Controller() {
        val values = FXCollections.observableArrayList("Dix", "Dongs", "Willies")
    }

    class TestController: Controller() {
        fun doLogging(text: String){
            println("Writing $text")
            Thread.sleep(2000)
        }
    }

}
