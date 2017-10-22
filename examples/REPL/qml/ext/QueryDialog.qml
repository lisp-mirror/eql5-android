import QtQuick 2.7
import QtQuick.Controls 2.0
import "." as Ext
import EQL5 1.0

Rectangle {
    anchors.fill: parent
    color: "#f0f0f0"
    visible: false
    z: 2

    Column {
        anchors.fill: parent

        Rectangle {
            width: parent.width
            height: cancel.height
            color: "#505050"

            Text {
                x: 8
                anchors.verticalCenter: parent.verticalCenter
                color: "white"
                font.bold: true
                font.pixelSize: 18
                text: "Query Dialog"
            }

            Ext.DialogButton {
                id: cancel
                x: parent.width - width
                text: "\uf00d"

                onClicked: {
                    queryInput.clear()
                    Lisp.call("dialogs:exited")
                }
            }
        }

        TextField {
            id: queryInput
            objectName: "query_input"
            width: parent.width
            font.family: "Droid Sans Mono"
            inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText

            onAccepted: Lisp.call("dialogs:exited")
        }

        Text {
            objectName: "query_text"
            width: parent.width
            leftPadding: 8
            rightPadding: 8
            topPadding: 8
        }
    }
}
