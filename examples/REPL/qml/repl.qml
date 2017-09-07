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
            cursorDelegate: cursor
            focus: true

            onCursorRectangleChanged: flickEdit.ensureVisible(cursorRectangle)

            Component.onCompleted: {
                Lisp.call(textDocument, "editor:set-text-document")
                Lisp.call("editor:set-delayed-focus")
            }

            Keys.onPressed: {
                if((event.key == Qt.Key_Return) || (event.key == Qt.Key_Enter)) {
                    Lisp.call("editor:return-pressed");
                }
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

        Component {
            id: cursor

            Rectangle {
                width: 3
                color: "blue"

                SequentialAnimation on opacity {
                    running: true
                    loops: Animation.Infinite

                    NumberAnimation { to: 0; duration: 500; easing.type: "OutQuad" }
                    NumberAnimation { to: 1; duration: 500; easing.type: "InQuad" }
                }
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

        Rectangle {
            color: "lightgray"
            objectName: "status_bar"
            border.width: 1
            border.color: "gray"
            width: main.width
            height: status.height
            anchors.bottom: parent.bottom
            visible: false

            Text {
                id: status
                objectName: "status"
                font.family: "Droid Sans Mono"
                font.pointSize: 18
            }
        }
    }

    function halfButtonWidth() {
        return evalLisp.width / 2 - 3
    }

    Rectangle {
        y: rectOutput.y - fontSize.height / 2
        width: fontSize.width
        height: fontSize.height
        anchors.horizontalCenter: rectOutput.horizontalCenter

        Row {
            id: fontSize
            padding: 4
            spacing: 6

            property int pointSize: evalLisp.font.pointSize

            Button {
                objectName: "font_smaller"
                width: halfButtonWidth()
                font.family: "Droid Sans Mono"
                text: "Aa"
                font.pointSize: fontSize.pointSize - 3
            }
            Button {
                objectName: "font_bigger"
                width: halfButtonWidth()
                font.family: "Droid Sans Mono"
                text: "Aa"
                font.pointSize: fontSize.pointSize + 3
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
            spacing: 6

            Button {
                objectName: "open_file"
                text: "File..."
            }
            Button {
                objectName: "save_file"
                text: "Save..."
            }
            Button {
                id: clear
                objectName: "clear"
                text: "Clear"
            }
            Row {
                spacing: 6

                Button {
                    objectName: "history_up"
                    width: halfButtonWidth()
                    text: "<<"
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
