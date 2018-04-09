// This is a modified version taken from the QML sources

import QtQuick 2.7
import QtQuick.Controls 2.0

ScrollBar {
    id: control
    implicitWidth: main.isPhone ? 15 : 22
    implicitHeight: implicitWidth
    padding: 1

    contentItem: Rectangle {
        implicitWidth: main.isPhone ? 4 : 8
        implicitHeight: implicitWidth
        radius: width / 2
        color: control.pressed ? "#c05050" : "#a0a0a0"
        opacity: 0.0

        states: State {
            name: "active"
            when: (control.active && control.size < 1.0)
            PropertyChanges { target: control.contentItem; opacity: 0.75 }
        }

        transitions: Transition {
            from: "active"
            SequentialAnimation {
                PauseAnimation { duration: 450 }
                NumberAnimation { target: control.contentItem; duration: 200; property: "opacity"; to: 0.0 }
            }
        }
    }
}
