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
        return (Screen.desktopAvailableHeight - Qt.inputMethod.keyboardRectangle.height - commandRect.height) / 2
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
            font.pixelSize: 18
            selectionColor: "firebrick"
            inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
            cursorDelegate: cursor

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
                // seems necessary to consistently move cursor by tapping
                edit.forceActiveFocus()
                edit.cursorPosition = edit.positionAt(mouse.x, mouse.y)
            }

            onPressAndHold: {
                Lisp.call("editor:copy-paste", edit.cursorPosition)
            }
        }

        Component {
            id: cursor

            Rectangle {
                width: 2
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
        width: buttonsTop.width
        height: buttonsTop.height
        anchors.horizontalCenter: parent.horizontalCenter
        opacity: 0.7

        Row {
            id: buttonsTop
            padding: 4
            spacing: 6

            Ext.MenuButton {
                objectName: "font_smaller"
                text: "\uf010"
                font.pixelSize: 15
            }
            Ext.MenuButton {
                objectName: "font_bigger"
                text: "\uf00e"
                font.pixelSize: 25
            }
        }
    }

    Rectangle {
        id: commandRect
        y: flickEdit.height
        width: parent.width
        height: command.font.pixelSize + 10
        border.width: 1
        border.color: command.focus ? "red" : "gray"

        TextEdit {
            id: command
            objectName: "command"
            anchors.fill: parent
            padding: 4
            font.family: "Droid Sans Mono"
            font.pixelSize: 18
            inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
            cursorDelegate: cursor
            focus: true

            Keys.onPressed: {
                if((event.key == Qt.Key_Return) || (event.key == Qt.Key_Enter)) {
                    event.accepted = true
                    Lisp.call("editor:eval-expression", text)
                    clear()
                }
            }
        }
    }

    Rectangle {
        id: rectOutput
        objectName: "rect_output"
        color: "lavender"
        y: flickEdit.height + commandRect.height
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
                font.pixelSize: 18
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
                font.pixelSize: 18
            }
        }
    }

    Rectangle {
        width: buttonsRight.width
        height: buttonsRight.height
        anchors.right: parent.right

        Column {
            id: buttonsRight
            padding: 4
            spacing: 8

            Ext.Button {
                objectName: "clear"
                text: "\uf014"
            }
            Ext.Button {
                objectName: "open_file"
                text: "\uf115"
            }
            Ext.Button {
                objectName: "save_file"
                text: "\uf0c7"
            }
            Ext.Button {
                objectName: "eval"
                text: "\u03bb" // lambda
            }
        }
    }

    Rectangle {
        width: buttonsBottom.width
        height: buttonsBottom.height
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.top: rectOutput.top
        opacity: 0.7

        Row {
            id: buttonsBottom
            padding: 4
            spacing: 6

            Ext.MenuButton {
                objectName: "history_up"
                text: "\uf100"
            }
            Ext.MenuButton {
                objectName: "history_down"
                text: "\uf101"
            }
        }
    }

    // not visible dialog / menu instances (will be called from Lisp)

    Ext.QueryDialog {
        objectName: "query_dialog"
    }

    Ext.DebugDialog {
        objectName: "debug_dialog"
    }

    Ext.ClipboardMenu {
        objectName: "clipboard_menu"
    }
}
