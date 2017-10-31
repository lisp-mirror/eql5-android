import QtQuick 2.7

Text {
    width: main.isPhone ? 37 : 50
    height: width
    horizontalAlignment: AlignHCenter
    verticalAlignment: AlignVCenter
    font.pixelSize: 1.2 * width
    opacity: 0.25
    scale: 1.2

    signal pressed()

    MouseArea {
        anchors.fill: parent
        onPressed: parent.pressed()
    }
}