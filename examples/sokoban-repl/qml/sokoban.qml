import QtQuick 2.6
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import "ext/" as Ext
import EQL5 1.0

Rectangle {
    id: main

    property bool isPhone: (Math.max(width, height) < 1000) // trivial but seems reliable

    FontLoader {
        id: fontAwesome
        source: "fonts/fontawesome-webfont.ttf"
    }

    Ext.FileBrowser {}

    Ext.Repl {}

    function isLandscape() { return (Screen.primaryOrientation == Qt.LandscapeOrientation) }

    Row {
        anchors.centerIn: parent
        // adapt 'level' and 'board' scale to screen size
        scale: isLandscape()
                   ? ((Screen.desktopAvailableHeight - 10) / board.height)
                   : ((Screen.desktopAvailableWidth - 10) / (board.width + 2 * level.width))

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
        id: buttons1
        objectName: "buttons1"
        spacing: isPhone ? 10 : 15
        padding: 10
        anchors.bottom: parent.bottom

        Ext.Button {
            objectName: "previous"
            text: "\uf048"
        }
        Ext.Button {
            objectName: "next"
            text: "\uf051"
        }
    }

    Row {
        id: buttons2
        objectName: "buttons2"
        spacing: isPhone ? 10 : 15
        padding: 10
        anchors.right: parent.right
        anchors.bottom: parent.bottom

        Ext.Button {
            objectName: "undo"
            text: "\uf112"
        }
        Ext.Button {
            objectName: "restart"
            text: "\uf0e2"
        }
        Ext.Button {
            objectName: "solve"
            text: "\uf04b"
        }
    }

    // container for arrow buttons
    Item {
        id: arrows
        y: buttons1.y - height - (main.isPhone ? 25 : 50)
        width: up.width * 3
        height: up.height * 3
        anchors.margins: 10
        anchors.horizontalCenter: buttons2.horizontalCenter

        Ext.ArrowButton {
            id: up
            objectName: "up"
            text: "\uf01b"
            anchors.horizontalCenter: parent.horizontalCenter
        }

        Ext.ArrowButton {
            objectName: "left"
            text: "\uf190"
            anchors.verticalCenter: parent.verticalCenter
        }

        Ext.ArrowButton {
            objectName: "right"
            text: "\uf18e"
            anchors.verticalCenter: parent.verticalCenter
            anchors.right: parent.right
        }

        Ext.ArrowButton {
            objectName: "down"
            text: "\uf01a"
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
}
