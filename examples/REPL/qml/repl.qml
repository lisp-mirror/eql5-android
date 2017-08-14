import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import 'ext/' as Ext
import EQL5 1.0

Rectangle {
    id: main
    objectName: "main"
    width: 900; height: 600 // for desktop

    function halfHeight() {
        return (Screen.desktopAvailableHeight - Qt.inputMethod.keyboardRectangle.height) / 2
    }

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
            focus: true

            onCursorRectangleChanged: flickEdit.ensureVisible(cursorRectangle)

            Component.onCompleted: {
                Lisp.call(textDocument, "editor:set-text-document")
                Lisp.call("editor:set-delayed-focus")
            }
        }

        MouseArea {
            anchors.fill: parent

            onPressed: {
                // seems necessary to move cursor by tapping
                edit.forceActiveFocus()
                edit.cursorPosition = edit.positionAt(mouse.x, mouse.y)
            }
        }
    }

    Rectangle {
        id: rectOutput
        objectName: "rect_output"
        color: "lavender"
        y: flickEdit.height
        width: main.width
        height: main.halfHeight()

        Ext.Flickable {
            id: flickOutput
            anchors.fill: parent
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

    function halfButtonWidth() {
        return evalLisp.width / 2 - 3
    }

    Rectangle {
        width: buttons.width
        height: buttons.height
        anchors.right: parent.right

        Column {
            id: buttons
            objectName: "buttons"
            padding: 4
            spacing: 6

            property int pointSize: evalLisp.font.pointSize

            Row {
                Button {
                    objectName: "font_smaller"
                    width: halfButtonWidth()
                    text: "Aa"
                    font.pointSize: buttons.pointSize - 3
                }
                Item {
                    width: 6; height: 1
                }
                Button {
                    objectName: "font_bigger"
                    width: halfButtonWidth()
                    text: "Aa"
                    font.pointSize: buttons.pointSize + 3
                }
            }
            Button {
                id: clear
                objectName: "clear"
                text: "Clear"
            }
            Button {
                objectName: "open_file"
                text: "File..."
            }
            Row {
                Button {
                    objectName: "history_up"
                    width: halfButtonWidth()
                    text: "<<"
                }
                Item {
                    width: 6; height: 1
                }
                Button {
                    objectName: "history_down"
                    width: halfButtonWidth()
                    text: ">>"
                }
            }
            Button {
                id: evalLisp
                height: clear.height * 2
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
}
