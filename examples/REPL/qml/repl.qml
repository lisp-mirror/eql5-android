import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import 'ext/' as Ext
import EQL5 1.0

Rectangle {
    id: main
    objectName: "main"
    width: 900; height: 600 // for desktop

    property bool isPhone: (Math.max(width, height) < 1000) // trivial but seems reliable

    function keyboardHeight() {
        var x = Qt.inputMethod.keyboardRectangle.width / Screen.width // needed for high dpi scaling
        return (Qt.inputMethod.keyboardRectangle.height / (x ? x : 1))
    }

    function halfHeight() {
        return (Screen.desktopAvailableHeight - keyboardHeight() - rectCommand.height) / 2
    }

    Rectangle {
        id: rectEdit
        width: main.width
        height: halfHeight()

        Ext.Flickable {
            id: flickEdit
            objectName: "flick_edit"
            anchors.fill: parent
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
                    Lisp.call(textDocument, "editor:set-text-document", objectName)
                    Lisp.call("editor:set-delayed-focus")
                }

                MouseArea {
                    width: Math.max(rectEdit.width, edit.paintedWidth)
                    height: Math.max(rectEdit.height, edit.paintedHeight)

                    onPressed: {
                        // seems necessary to consistently move cursor by tapping
                        edit.forceActiveFocus()
                        edit.cursorPosition = edit.positionAt(mouse.x, mouse.y)
                        Qt.inputMethod.show() // needed for edge case (since we have 2 input fields)
                        Lisp.call("editor:set-focus-editor", edit.objectName)
                    }

                    onPressAndHold: Lisp.call("editor:copy-paste", edit.cursorPosition)
                }
            }

            Component {
                id: cursor

                Rectangle {
                    width: 2
                    color: "blue"
                    visible: parent.activeFocus

                    SequentialAnimation on opacity {
                        running: true
                        loops: Animation.Infinite

                        NumberAnimation { to: 0; duration: 500; easing.type: "OutQuad" }
                        NumberAnimation { to: 1; duration: 500; easing.type: "InQuad" }
                    }
                }
            }
        }
    }

    Rectangle {
        id: buttonsTop
        y: -height // hidden
        width: rowButtonsTop.width
        height: rowButtonsTop.height
        anchors.horizontalCenter: parent.horizontalCenter
        opacity: 0.7

        Row {
            id: rowButtonsTop
            padding: 4
            spacing: 6

            Ext.MenuButton {
                objectName: "undo"
                text: "\uf0e2"
            }
            Ext.MenuButton {
                objectName: "redo"
                text: "\uf01e"
            }
            Ext.MenuButton {
                objectName: "font_smaller"
                text: "\uf010"
                font.pixelSize: main.isPhone ? 10 : 15
            }
            Ext.MenuButton {
                objectName: "font_bigger"
                text: "\uf00e"
                font.pixelSize: main.isPhone ? 16 : 25
            }
        }
    }

    Rectangle {
        id: rectCommand
        y: flickEdit.height
        width: parent.width
        height: command.font.pixelSize + 12
        border.width: 1.5
        border.color: command.focus ? "#0066ff" : "lightgray"

        Ext.Flickable {
            id: flickCommand
            anchors.fill: parent
            contentWidth: command.paintedWidth
            contentHeight: command.paintedHeight

            TextEdit {
                id: command
                objectName: "command"
                width: flickCommand.width
                height: flickCommand.height
                padding: 4
                font.family: "Droid Sans Mono"
                font.pixelSize: 18
                selectionColor: "firebrick"
                inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
                cursorDelegate: cursor
                focus: true

                onCursorRectangleChanged: flickCommand.ensureVisible(cursorRectangle)

                Component.onCompleted: Lisp.call(textDocument, "editor:set-text-document", objectName)

                MouseArea {
                    width: Math.max(rectCommand.width, command.paintedWidth)
                    height: Math.max(rectCommand.height, command.paintedHeight)

                    onPressed: {
                        // seems necessary to consistently move cursor by tapping
                        command.forceActiveFocus()
                        command.cursorPosition = command.positionAt(mouse.x, mouse.y)
                        Qt.inputMethod.show() // needed for edge case (since we have 2 input fields)
                        Lisp.call("editor:set-focus-editor", command.objectName)
                    }

                    onPressAndHold: Lisp.call("editor:copy-paste", command.cursorPosition)
                }
            }
        }
    }

    Rectangle {
        id: rectOutput
        objectName: "rect_output"
        color: "lavender"
        y: flickEdit.height + rectCommand.height
        width: main.width
        height: halfHeight()

        Ext.Flickable {
            id: flickOutput
            anchors.fill: parent
            contentWidth: output.paintedWidth
            contentHeight: output.paintedHeight

            onFlickStarted: output.forceActiveFocus()

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

    Ext.MenuButton {
        id: showMenu
        objectName: "show_menu"
        x: parent.width - width - 4
        y: 4
        opacity: 0.7
        text: "\uf142"

        onClicked: {
            showButtonsTop.start()
            showButtonsRight.start()
            Lisp.call("editor:start-menu-timer")
        }
    }

    Rectangle {
        id: buttonsRight
        x: parent.width // hidden
        width: colButtonsRight.width
        height: colButtonsRight.height

        Column {
            id: colButtonsRight
            padding: 4
            spacing: 6

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
        id: buttonsBottom
        width: rowButtonsBottom.width
        height: rowButtonsBottom.height
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.top: rectOutput.top
        opacity: 0.7
        visible: command.activeFocus

        Row {
            id: rowButtonsBottom
            padding: 4
            spacing: 6

            Ext.MenuButton {
                objectName: "history_back"
                text: "\uf100"
            }
            Ext.MenuButton {
                objectName: "history_forward"
                text: "\uf101"
            }
        }
    }

    // animations for showing/hiding editor menu buttons

    NumberAnimation {
        id: showButtonsTop
        target: buttonsTop
        property: "y"
        from: -buttonsTop.height
        to: 0
        duration: 500
        easing.type: Easing.OutExpo
    }

    NumberAnimation {
        id: showButtonsRight
        target: buttonsRight
        property: "x"
        from: buttonsRight.parent.width
        to: buttonsRight.parent.width - buttonsRight.width
        duration: 500
        easing.type: Easing.OutExpo
    }

    NumberAnimation {
        objectName: "hide_buttons_top"
        target: buttonsTop
        property: "y"
        from: 0
        to: -buttonsTop.height
        duration: 500
        easing.type: Easing.InExpo
    }

    NumberAnimation {
        objectName: "hide_buttons_right"
        target: buttonsRight
        property: "x"
        from: buttonsRight.parent.width - buttonsRight.width
        to: buttonsRight.parent.width
        duration: 500
        easing.type: Easing.InExpo
    }

    // arrow buttons (cursor movement)

    Rectangle {
        id: arrowRect
        width: arrows.width + 50
        height: width + 20
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        color: "transparent"
        visible: (edit.focus || command.focus)

        MouseArea {
            anchors.fill: parent
            onPressed: Lisp.call("editor:ensure-focus")
        }

        Item {
            id: arrows
            width: up.width * 3
            height: width
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.verticalCenter: parent.verticalCenter

            Ext.ArrowButton {
                id: up
                objectName: "up"
                text: "\uf139"
                anchors.horizontalCenter: parent.horizontalCenter
            }
            
            Ext.ArrowButton {
                objectName: "left"
                text: "\uf137"
                anchors.verticalCenter: parent.verticalCenter
            }

            Ext.ArrowButton {
                objectName: "right"
                text: "\uf138"
                anchors.verticalCenter: parent.verticalCenter
                anchors.right: parent.right
            }

            Ext.ArrowButton {
                objectName: "down"
                text: "\uf13a"
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.bottom: parent.bottom
            }
        }
    }

    Button {
        objectName: "keyboard"
        y: Screen.desktopAvailableHeight - height - 10
        width: main.isPhone ? 70 : 100
        height: main.isPhone ? 50 : 70
        anchors.horizontalCenter: parent.horizontalCenter
        font.pixelSize: height
        text: "\uf11c"
        opacity: 0.35
        focusPolicy: Qt.NoFocus
        visible: arrowRect.visible
    }

    // icon font

    FontLoader {
        id: fontAwesome
        source: "fonts/fontawesome-webfont.ttf"
    }

    // not visible dialog / menu instances (will be called from Lisp)

    Ext.FileBrowser {
        objectName: "file_browser"
    }

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
