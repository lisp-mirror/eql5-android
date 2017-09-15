import QtQuick 2.6
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import "ext/" as Ext
import EQL5 1.0

Rectangle {
    id: main
    width: 900; height: 600

    Row {
        // adapt 'level' and 'board' scale to screen size
        scale: (Screen.primaryOrientation == Qt.LandscapeOrientation)
                   ? ((Screen.desktopAvailableHeight - 2 * buttons.height) / board.height)
                   : ((Screen.desktopAvailableWidth - 2 * arrows.width - 2 * level.width) / board.width)
        anchors.centerIn: parent

        Slider {
            id: level
            objectName: "level"
            height: board.height
            orientation: Qt.Vertical
            stepSize: 1.0

            onValueChanged: Lisp.call("qsoko:set-maze")
        }

        Rectangle {
            id: board
            objectName: "board"
            width: 512; height: 512
            color: "lightsteelblue"
        }

        // dummy to have it exactly centered
        Item {
            width: level.width
            height: level.height
        }
    }

    Row {
        id: buttons
        objectName: "buttons"
        spacing: 15
        padding: 15
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom

        Ext.Button {
            objectName: "previous"
            source: "img/previous.png"
        }
        Ext.Button {
            objectName: "next"
            source: "img/next.png"
        }
        Ext.Button {
            objectName: "undo"
            source: "img/undo.png"
        }
        Ext.Button {
            objectName: "restart"
            source: "img/restart.png"
        }
        Ext.Button {
            objectName: "solve"
            source: "img/solve.png"
        }
    }

    // container for arrow buttons
    Item {
        id: arrows
        width: up.width * 3
        height: up.height * 3
        anchors.margins: 15
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter

        Ext.Button {
            id: up
            objectName: "up"
            source: "img/up.png"
            anchors.horizontalCenter: parent.horizontalCenter
        }

        Ext.Button {
            objectName: "left"
            source: "img/left.png"
            anchors.verticalCenter: parent.verticalCenter
        }

        Ext.Button {
            objectName: "right"
            source: "img/right.png"
            anchors.verticalCenter: parent.verticalCenter
            anchors.right: parent.right
        }

        Ext.Button {
            objectName: "down"
            source: "img/down.png"
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.bottom: parent.bottom
        }
    }

    // level change animations

    Ext.ScaleAnimator {
	objectName: "zoom_board_out"
        target: board
        from: 1.0
        to: 0.0
        duration: 250
    }

    Ext.ScaleAnimator {
	objectName: "zoom_board_in"
        target: board
        from: 0.0
        to: 1.0
        duration: 250
    }

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

            onCheckedChanged: Lisp.call("qsoko:show-repl", checked)
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
