import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import "." as Ext
import EQL5 1.0

Rectangle {
    id: debugDialog
    objectName: "debug_dialog"
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
                text: "Debug Dialog"
            }

            Ext.DialogButton {
                id: back
                x: parent.width - width
                text: "\uf105"

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
            font.pixelSize: debugInput.font.pixelSize - (main.isPhone ? 4 : 2)
            text: ":r1 etc. restart / :h help / :q quit"
        }

        Rectangle {
            id: line
            width: parent.width
            height: 1
            color: "#d0d0d0"
        }

        Flickable {
            id: flickText
            width: parent.width
            height: debugDialog.height
                    - back.height
                    - debugInput.height
                    - label.height
                    - line.height
                    - main.keyboardHeight()
            contentWidth: text.paintedWidth
            contentHeight: text.paintedHeight + text.topPadding
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
