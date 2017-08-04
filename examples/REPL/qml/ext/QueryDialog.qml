import QtQuick 2.3
import QtQuick.Dialogs 1.2
import QtQuick.Controls 2.0
import EQL5 1.0

Dialog {
    title: "Query Dialog"
    standardButtons: StandardButton.Ok | StandardButton.Cancel

    function exit() { Lisp.call("dialogs:exit-event-loop") }

    onAccepted: exit()
    onRejected: { input.clear(); exit() }
    
    Column {
        anchors.fill: parent

        Text {
            objectName: "query_text"
        }

        TextField {
            id: input
            objectName: "query_input"
            width: parent.width
            inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
            focus: true
        }
    }
}

