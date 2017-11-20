import QtQuick 2.7

Text {
    width: main.isPhone ? 40 : 50
    height: width
    horizontalAlignment: Text.AlignHCenter
    verticalAlignment: Text.AlignVCenter
    font.pixelSize: 1.2 * width
    opacity: 0.20
    scale: 1.2

    signal pressed()

    MouseArea {
        anchors.fill: parent
        onPressed: parent.pressed()
    }
}
