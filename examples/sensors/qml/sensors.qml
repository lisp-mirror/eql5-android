import QtQuick 2.7
import QtSensors 5.7
import "ext/" as Ext
import EQL5 1.0

Rectangle {
    id: main
    objectName: "main"
    color: "skyblue"

    property bool isPhone: (Math.max(width, height) < 1000) // trivial but seems reliable

    FontLoader {
        id: fontAwesome
        source: "fonts/fontawesome-webfont.ttf"
    }

    Ext.FileBrowser {}

    Ext.Repl {}

    Text {
        padding: 10
        anchors.fill: parent
        font.pixelSize: 20
        color: "navy"
        text: "<br>available sensors:<br><ul><li>" + QmlSensors.sensorTypes().join("<li>") + "</ul>"
    }

    Accelerometer {
        id: accel
        objectName: "accel"
        dataRate: 75 // in Hertz
        active: true

        onReadingChanged: Lisp.call("sensors:move-bubble", accel.reading.x, accel.reading.y, accel.reading.z)
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
}
