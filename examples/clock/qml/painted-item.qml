import QtQuick 2.7
import EQL5 1.0

Rectangle {
    color: "black"
    width: 500; height: 500 // must be defined (for QQuickWidget)

    PaintedItem {
        id: clock
        objectName: "clock"
        anchors.fill: parent

        SequentialAnimation {
            running: true
            loops: Animation.Infinite

            ScaleAnimator {
                target: clock
                from: 0.9; to: 1.0
                duration: 500
                easing.type: Easing.InOutSine
            }

            ScaleAnimator {
                target: clock
                from: 1.0; to: 0.9
                duration: 500
                easing.type: Easing.InOutSine
            }
        }
    }
}
