import QtQuick 2.0

Image {
    scale: 1.2
    opacity: 0.8

    signal pressed()

    MouseArea {
        anchors.fill: parent
        onPressed: parent.pressed()
    }
}

