import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import 'ext/' as Ext
import EQL5 1.0

Rectangle {
    id: main
    width: 800; height: 500 // needed for desktop

    Column {

        Ext.Flickable {
            id: flickEdit
            objectName: "flickEdit"

            width: main.width
            height: (main.height - Qt.inputMethod.keyboardRectangle.height) / 2
            contentWidth: edit.paintedWidth
            contentHeight: edit.paintedHeight

            TextEdit {
                id: edit
                objectName: "edit"

                width: flickEdit.width
                height: flickEdit.height
                font.family: "Droid Sans Mono"
                font.pointSize: 18
                inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
                wrapMode: TextEdit.Wrap
                focus: true

                onCursorRectangleChanged: flickEdit.ensureVisible(cursorRectangle)
                Component.onCompleted: Lisp.call(textDocument, "editor:set-text-document")
            }
        }

        Rectangle {
            width: main.width
            height: 1
            color: "gray"
        }

        Ext.Flickable {
            id: flickOutput
            objectName: "flickOutput"

            width: main.width
            height: (main.height - Qt.inputMethod.keyboardRectangle.height) / 2
            contentWidth: output.paintedWidth
            contentHeight: output.paintedHeight

            TextEdit {
                id: output
                objectName: "output"

                width: flickOutput.width
                height: flickOutput.height
                font.family: "Droid Sans Mono"
                font.pointSize: 18
                readOnly: true
                textFormat: TextEdit.RichText

                onCursorRectangleChanged: flickOutput.ensureVisible(cursorRectangle)
            }
        }
    }

    Column {
        id: buttons
        objectName: "buttons"

        padding: 2
        spacing: 2
        anchors.right: parent.right

        property int pointSize: evalLisp.font.pointSize

        Button {
            id: clear
            objectName: "clear"
            text: "Clear"
        }
        Button {
            objectName: "font_bigger"
            text: "Aa"
            font.pointSize: parent.pointSize + 2
        }
        Button {
            objectName: "font_smaller"
            text: "Aa"
            font.pointSize: parent.pointSize - 2
        }
        Button {
            objectName: "history_up"
            text: "Up"
        }
        Button {
            objectName: "history_down"
            text: "Down"
        }
        Button {
            id: evalLisp
            objectName: "eval"
            height: clear.height * 2
            text: "<b>Eval</b>"
        }
    }

    // not visible dialog instances (will be called from Lisp)

    Ext.QueryDialog {
        objectName: "query_dialog"
    }

    Ext.DebugDialog {
        objectName: "debug_dialog"
    }
}
