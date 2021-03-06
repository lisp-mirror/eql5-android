// for (c) please see COPYING.txt

import QtQuick 2.6
import QtQuick.Controls 2.0
import "ext/" as Ext
import EQL5 1.0

Rectangle {
    id: game
    objectName: "game"
    width: 420; height: 500

    property bool running: true
    property int difficulty: 10 // chance it will actually think

    Keys.onPressed: {
        if(event.key == Qt.Key_Back) {
            event.accepted = true
            Lisp.call("eql:back-pressed")
        }
    }

    Image {
        id: board_image
        objectName: "board_image"
        anchors.centerIn: parent
        source: "../pics/board.png"
    }

    Column {
        id: display
        objectName: "display"
        anchors.centerIn: parent

        Grid {
            id: board
            objectName: "board"
            width: board_image.width; height: board_image.height
            columns: 3

            Repeater {
                model: 9

                Ext.TicTac {
                    width: board.width / 3
                    height: board.height / 3

                    onClicked: Lisp.call("logic:tic-tac-clicked", index)
                }
            }
        }
    }

    Row {
        spacing: 5
        padding: 10
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom

        Button {
            text: "Hard"
            down: game.difficulty == 10
            onClicked: { game.difficulty = 10 }
        }
        Button {
            text: "Moderate"
            down: game.difficulty == 8
            onClicked: { game.difficulty = 8 }
        }
        Button {
            text: "Easy"
            down: game.difficulty == 2
            onClicked: { game.difficulty = 2 }
        }
    }

    Text {
        id: message_display
        objectName: "message_display"
        anchors.centerIn: parent
        color: "blue"
        style: Text.Outline; styleColor: "white"
        font.pixelSize: 50; font.bold: true
        visible: false

        Timer {
            running: message_display.visible
            onTriggered: Lisp.call("logic:restart-game")
        }
    }
}
