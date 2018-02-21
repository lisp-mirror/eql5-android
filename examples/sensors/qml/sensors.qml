import QtQuick 2.7
import QtQuick.Controls 2.0
import QtSensors 5.7
import "ext/" as Ext
import EQL5 1.0

StackView {
    id: main
    objectName: "main"
    initialItem: mainRect

    property bool isPhone: (Math.max(width, height) < 1000) // trivial but seems reliable

    // show/hide dialogs (using StackView)
    // (we can't call 'push()'/'pop()' from Lisp: no C++ interface, so we call JS from Lisp instead)

    function pushFileDialog() { main.push(fileDialogInstance) }
    function popDialog()      { main.pop() }

    Ext.FileBrowser { id: fileDialogInstance }

    Keys.onPressed: {
        if(event.key == Qt.Key_Back) {
            console.log("back pressed")
            event.accepted = true
            Lisp.call("eql:back-pressed")
        }
    }

    FontLoader { id: fontAwesome; source: "fonts/fontawesome-webfont.ttf" }

    Rectangle {
        id: mainRect
        color: "skyblue"

        Ext.Repl {}

        Column {
            anchors.fill: parent

            Text {
                padding: 10
                color: "navy"
                text: "<br><b>Available sensors:</b><br><ul><li>" + QmlSensors.sensorTypes().sort().join("<li>") + "</ul>"
            }

            Text {
                padding: 10
                id: azimuth
                objectName: "azimuth"
                width: parent.width
                font.pixelSize: 74
                horizontalAlignment: Text.AlignHCenter
                text: "?"
            }

            Text {
                width: parent.width
                horizontalAlignment: Text.AlignHCenter
                text: "<b>azimuth</b><br><br>for best accuracy,<br>the bubble should not move"
            }
        }

        Image {
            id: bubble
            objectName: "bubble"
            source: "img/bubble.svg"
            smooth: true
            x: (main.width - bubble.width) / 2
            y: (main.height - bubble.width) / 2
            z: 1
        }

        // sensors (are read from Lisp, using a timer)

        Accelerometer {
            id: accel
            objectName: "accel"
            active: true

            Component.onCompleted: {
                if(active) {
                    Lisp.call(reading, "sensors:set-accel-reading") // for speed optimization
                }
            }
        }

        Compass {
            id: compass
            objectName: "compass"
            active: true
        }
    }
}
