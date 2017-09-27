import QtQuick 2.6
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import "ext/" as Ext
import EQL5 1.0

Rectangle {
    id: main
    objectName: "main"
    color: "lightcyan"

    // your QML



    // REPL
    
    Row {
        z: 1
        anchors.right: parent.right

        Text {
            text: "REPL"
            anchors.verticalCenter: showRepl.verticalCenter
        }
        Switch {
            id: showRepl

            onCheckedChanged: Lisp.call("my:show-repl", checked)
        }
    }

    Column {
        objectName: "repl_container"
        visible: false

        Rectangle {
            width: main.width
            height: main.height / 4
            color: "#101010"
            opacity: 0.8

            Ext.Flickable {
                id: flickOutput
                anchors.fill: parent
                contentWidth: replOutput.paintedWidth
                contentHeight: replOutput.paintedHeight

                TextEdit {
                    id: replOutput
                    objectName: "repl_output"
                    width: flickOutput.width
                    height: flickOutput.height
                    font.pointSize: 16
                    textFormat: TextEdit.RichText
                    color: "white"
                    readOnly: true

                    onCursorRectangleChanged: flickOutput.ensureVisible(cursorRectangle)
                }
            }
        }

        Row {
            width: main.width

            TextField {
                id: replInput
                objectName: "repl_input"
                width: parent.width - historyUp.width - historyDown.width
                font.family: "Droid Sans Mono"
                color: "white"
                opacity: 0.8
                inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
                focus: showRepl.checked

                background: Rectangle {
                    color: "#101010"
                    border.width: 1
                    border.color: "orange"
                }

                onAccepted: Lisp.call("eval:eval-in-thread", text)
            }
            Button {
                id: historyUp
                objectName: "history_up"
                width: 55
                height: replInput.height
                text: "<<"

                onClicked: Lisp.call("eval:history-move", "up")
            }
            Rectangle {
                width: 1
                height: replInput.height
                color: "#101010"
                opacity: 0.8
            }
            Button {
                id: historyDown
                objectName: "history_down"
                width: 55
                height: replInput.height
                text: ">>"

                onClicked: Lisp.call("eval:history-move", "down")
            }
        }

        Rectangle {
            width: main.width
            height: 1
            color: "#101010"
            opacity: 0.8
        }
    }
}
