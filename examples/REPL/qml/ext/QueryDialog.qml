import QtQuick 2.7
import QtQuick.Controls 2.0
import "." as Ext
import EQL5 1.0

Rectangle {
    id: queryDialog
    objectName: "query_dialog"
    color: "#f0f0f0"
    visible: false

    Column {
        anchors.fill: parent

        Rectangle {
            id: menu
            width: parent.width
            height: back.height
            color: "#f0f0f0"

            Text {
                x: 8
                anchors.verticalCenter: parent.verticalCenter
                font.pixelSize: 18
                font.bold: true
                text: "Query Dialog"
            }

            Ext.DialogButton {
                id: back
                x: parent.width - width
                text: "\uf105"

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
            height: queryDialog.height - menu.height - queryInput.height - main.keyboardHeight()
            leftPadding: 8
            rightPadding: 8
            topPadding: 8
            font.pixelSize: queryInput.font.pixelSize - (main.isPhone ? 2 : 0)
        }
    }
}
