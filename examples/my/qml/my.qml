// all interesting QML modules (for playing around)

import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import QtQuick.Layouts 1.3
import QtMultimedia 5.7
import QtSensors 5.7
import QtPositioning 5.7
import QtWebView 1.1
import "ext/" as Ext
import EQL5 1.0

Rectangle {
    id: main
    objectName: "main"
    color: "lightcyan"

    property bool isPhone: (Math.max(width, height) < 1000) // trivial but seems reliable

    Ext.Repl {}

    // your QML

    Ext.PositionSource {
        objectName: "pos_src"
    }
}
