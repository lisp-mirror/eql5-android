import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import QtQuick.Layouts 1.3
import "ext/" as Ext
import EQL5 1.0

Rectangle {
    id: main
    objectName: "main"
    color: "lightcyan"

    property bool isPhone: (Math.max(width, height) < 1000) // trivial but seems reliable

    FontLoader {
        id: fontAwesome
        source: "fonts/fontawesome-webfont.ttf"
    }

    Ext.FileBrowser {}

    Ext.Repl {}

    // your QML

}
