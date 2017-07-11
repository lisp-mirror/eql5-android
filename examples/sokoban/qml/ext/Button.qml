import QtQuick 2.0

Image {
    signal clicked()

    MouseArea {
        anchors.fill: parent
        onClicked: { parent.clicked() }
    }
}

