import QtQuick 2.7
import QtQuick.Controls 2.0
import EQL5 1.0
import "." as Ext

Item {
    id: repl
    z: 1
    anchors.fill: parent

    Ext.FileBrowser {}

    Row {
        z: 1
        anchors.right: parent.right

        Text {
            text: "REPL"
            anchors.verticalCenter: showRepl.verticalCenter
        }
        Switch {
            id: showRepl

            onCheckedChanged: Lisp.call("sensors:show-repl", checked)
        }
    }

    Column {
        id: replContainer
        objectName: "repl_container"
        visible: false

        Rectangle {
            width: repl.parent.width
            height: repl.parent.height / 4
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
                    textFormat: TextEdit.RichText
                    font.pixelSize: 14
                    color: "white"
                    readOnly: true

                    onCursorRectangleChanged: flickOutput.ensureVisible(cursorRectangle)
                }
            }
        }

        Row {
            width: repl.parent.width

            TextField {
                id: replInput
                objectName: "repl_input"
                width: repl.parent.width - historyBack.width - historyForward.width
                font.family: "Droid Sans Mono"
                font.pixelSize: 14
                color: "white"
                opacity: 0.8
                inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
                focus: showRepl.checked

                background: Rectangle {
                    color: "#101010"
                    border.width: 1
                    border.color: parent.focus ? "orange" : "gray"
                }

                onAccepted: Lisp.call("eval:eval-in-thread", text)
            }
            Button {
                id: historyBack
                objectName: "history_back"
                width: main.isPhone ? 40 : 55
                height: replInput.height
                focusPolicy: Qt.NoFocus
                font.pixelSize: 26
                text: "\uf100"

                onClicked: Lisp.call("eval:history-move", "back")
            }
            Rectangle {
                width: 1
                height: replInput.height
                color: "#101010"
                opacity: 0.8
            }
            Button {
                id: historyForward
                objectName: "history_forward"
                width: historyBack.width
                height: replInput.height
                focusPolicy: Qt.NoFocus
                font.pixelSize: 26
                text: "\uf101"

                onClicked: Lisp.call("eval:history-move", "forward")
            }
        }

        Rectangle {
            width: repl.parent.width
            height: 1
            color: "#101010"
            opacity: 0.8
        }
    }

    Row {
        y: replContainer.height + 5
        anchors.horizontalCenter: parent.horizontalCenter
        spacing: 15
        visible: replInput.focus

        // cursor back
        Ext.ArrowButton {
            text: "\uf137"

            onPressed:      replInput.cursorPosition--
            onPressAndHold: replInput.cursorPosition = 0
        }

        // cursor forward
        Ext.ArrowButton {
            text: "\uf138"

            onPressed:      replInput.cursorPosition++
            onPressAndHold: replInput.cursorPosition = replInput.length
        }
    }
}
