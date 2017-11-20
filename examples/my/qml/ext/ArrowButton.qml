import QtQuick 2.7

Text {
    width: main.isPhone ? 40 : 55
    height: width
    horizontalAlignment: Text.AlignHCenter
    verticalAlignment: Text.AlignVCenter
    font.pixelSize: 1.2 * width
    opacity: 0.20

    signal pressed()
    signal pressAndHold()

    MouseArea {
        anchors.fill: parent
        onPressed: parent.pressed()
        onPressAndHold: parent.pressAndHold()
    }
}
