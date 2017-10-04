import QtQuick 2.6
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import "ext/" as Ext
import EQL5 1.0

Rectangle {
    id: main
    width: 900; height: 600

    Ext.Repl {}

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
}
