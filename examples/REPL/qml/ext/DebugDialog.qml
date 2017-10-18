import QtQuick 2.3
import QtQuick.Dialogs 1.2
import QtQuick.Controls 2.0
import EQL5 1.0

Dialog {
    title: "Debug Dialog"
    standardButtons: StandardButton.Ok | StandardButton.Cancel

    function exit() { Lisp.call("dialogs:exited") }

    onAccepted: exit()
    onRejected: { input.clear(); exit() }
    
    Column {
        Flickable {
            id: flickText
            width: 500
            height: 250
            contentWidth: text.paintedWidth
            contentHeight: text.paintedHeight
            clip: true

            TextEdit {
                id: text
                objectName: "debug_text"
                width: flickText.width
                height: flickText.height
                textFormat: TextEdit.RichText
                readOnly: true
            }
        }

        Text {
            id: command
            text: "Enter debug command (:r1 for restart 1 etc., :h for help)"
        }

        TextField {
            id: input
            objectName: "debug_input"
            width: parent.width
            text: ":q"
            font.family: "Droid Sans Mono"
            inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
        }
    }
}
