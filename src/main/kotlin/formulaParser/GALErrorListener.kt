package formulaParser

import javafx.beans.property.StringProperty
import javafx.beans.value.ObservableStringValue
import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer

class GALErrorListener(val callbackTarget: StringProperty) : BaseErrorListener() {


    override fun syntaxError(recognizer: Recognizer<*, *>?, offendingSymbol: Any?,
                             line: Int, charPositionInLine: Int, msg: String?, e: RecognitionException?) {
        callbackTarget.value = "Failed to parse $msg"
    }

}