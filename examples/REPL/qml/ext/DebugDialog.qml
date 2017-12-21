import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import "." as Ext
import EQL5 1.0

Rectangle {
    id: dialog
    anchors.fill: parent
    color: "#f0f0f0"
    visible: false
    z: 2

    Column {
        anchors.fill: parent

        Rectangle {
            id: menu
            width: parent.width
            height: cancel.height
            color: "#505050"

            Text {
                x: 8
                anchors.verticalCenter: parent.verticalCenter
                color: "white"
                font.bold: true
                font.pixelSize: 18
                text: "Debug Dialog"
            }

            Ext.DialogButton {
                id: cancel
                x: parent.width - width
                text: "\uf00d"

                onClicked: {
                    debugInput.text = ":q"
                    Lisp.call("dialogs:exited")
                }
            }
        }

        TextField {
            id: debugInput
            objectName: "debug_input"
            width: parent.width
            font.family: "Droid Sans Mono"
            inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
            text: ":q"

            onAccepted: Lisp.call("dialogs:exited")
        }

        Text {
            id: label
            width: parent.width
            leftPadding: 8
            rightPadding: 8
            topPadding: 8
            bottomPadding: 8
            font.family: "Droid Sans Mono"
            font.pixelSize: debugInput.font.pixelSize - 4
            text: ":r1 etc. restart / :h help / :q quit"
        }

        Rectangle {
            id: line
            width: parent.width
            height: 1
            color: "gray"
        }

        Flickable {
            id: flickText
            width: parent.width
            height: dialog.height
                    - cancel.height
                    - debugInput.height
                    - label.height
                    - line.height
                    - main.keyboardHeight()
            contentWidth: text.paintedWidth
            contentHeight: text.paintedHeight
            clip: true

            TextEdit {
                id: text
                objectName: "debug_text"
                width: flickText.width
                height: flickText.height
                leftPadding: 8
                rightPadding: 8
                topPadding: 8
                font.pixelSize: debugInput.font.pixelSize - (main.isPhone ? 2 : 0)
                textFormat: TextEdit.RichText
                readOnly: true
            }
        }
    }
}
