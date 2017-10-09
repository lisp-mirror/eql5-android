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

    Ext.Repl {}

    // your QML

    FocusScope {

        Row {

            TextEdit {
                width: 200
                height: 200
                text: "abc"
            }

            Button {
                text: "OK"
            }
        }
    }
}
