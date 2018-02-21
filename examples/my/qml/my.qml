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
            event.accepted = true
            Lisp.call("eql:back-pressed")
        }
    }

    FontLoader { id: fontAwesome; source: "fonts/fontawesome-webfont.ttf" }

    Rectangle {
        id: mainRect
        color: "lightcyan"

        Ext.Repl {}

        // your QML

    }
}
