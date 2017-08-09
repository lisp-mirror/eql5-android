import QtQuick 2.7
import QtQuick.Controls 2.0
import 'ext/' as Ext
import EQL5 1.0

Rectangle {
    id: main
    objectName: "main"

    width: 800; height: 500 // for desktop

    function halfHeight() {
        return (height - Qt.inputMethod.keyboardRectangle.height) / 2
    }

    // call show() manually for correct resizing of flickables on startup
    Component.onCompleted: Qt.inputMethod.show()

    Column {

        Ext.Flickable {
            id: flickEdit
            objectName: "flick_edit"

            width: main.width
            height: main.halfHeight()
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

                Component.onCompleted: {
                    Lisp.call(textDocument, "editor:set-text-document")
                }
            }
        }

        Rectangle {
            width: main.width
            height: 1
            color: "gray"
        }

        Ext.Flickable {
            id: flickOutput
            objectName: "flick_output"

            width: main.width
            height: main.halfHeight()
            contentWidth: output.paintedWidth
            contentHeight: output.paintedHeight

            TextEdit {
                id: output
                objectName: "output"

                width: flickOutput.width
                height: flickOutput.height
                font.pointSize: 18
                textFormat: TextEdit.RichText
                readOnly: true

                onCursorRectangleChanged: flickOutput.ensureVisible(cursorRectangle)
            }
        }
    }

    Rectangle {
        width: buttons.width
        height: buttons.height
        anchors.right: parent.right

        Column {
            id: buttons
            objectName: "buttons"

            padding: 4
            spacing: 7

            property int pointSize: evalLisp.font.pointSize

            Button {
                objectName: "font_bigger"
                text: "Aa"
                font.pointSize: parent.pointSize + 3
            }
            Button {
                objectName: "font_smaller"
                text: "Aa"
                font.pointSize: parent.pointSize - 3
            }
            Button {
                objectName: "clear"
                text: "Clear"
            }
            Button {
                objectName: "open_file"
                text: "File..."
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
                text: "<b>Eval</b>"
            }
        }
    }

    // not visible dialog instances (will be called from Lisp)

    Ext.QueryDialog {
        objectName: "query_dialog"
    }

    Ext.DebugDialog {
        objectName: "debug_dialog"
    }

    Ext.FileDialog {
        objectName: "file_dialog"
    }
}
