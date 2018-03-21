// This is a modified version taken from the QML sources

import QtQuick 2.7
import QtQuick.Controls 2.0

ScrollBar {
    id: control
    implicitWidth: 6
    implicitHeight: 6
    padding: 1

    contentItem: Rectangle {
        implicitWidth: 4
        implicitHeight: 4
        radius: width / 2
        color: control.pressed ? "#b02020" : "#303030"
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
