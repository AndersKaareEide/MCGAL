package formulaParser

import javafx.beans.property.StringProperty
import javafx.beans.value.ObservableStringValue
import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer

class GALErrorListener(private val callbackTarget: (String) -> Unit) : BaseErrorListener() {

    override fun syntaxError(recognizer: Recognizer<*, *>?, offendingSymbol: Any?,
                             line: Int, charPositionInLine: Int, msg: String?, e: RecognitionException?) {
        callbackTarget("Failed to parse $msg")
    }

}