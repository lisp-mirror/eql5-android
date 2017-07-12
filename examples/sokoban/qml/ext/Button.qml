import QtQuick 2.0

Image {
    signal pressed()

    MouseArea {
        anchors.fill: parent
        onPressed: { parent.pressed() }
    }
}

