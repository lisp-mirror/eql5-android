import QtQuick 2.7

Text {
    width: main.isPhone ? 37 : 50
    height: width
    horizontalAlignment: Text.AlignHCenter
    verticalAlignment: Text.AlignVCenter
    font.pixelSize: 1.2 * width
    opacity: 0.2
    scale: 1.2

    signal pressed()
    signal pressAndHold()

    MouseArea {
        anchors.fill: parent
        onPressed: parent.pressed()
        onPressAndHold: parent.pressAndHold()
    }
}
