import QtQuick 2.7
import QtQuick.Controls 2.0
import Qt.labs.folderlistmodel 2.1
import "." as Ext
import EQL5 1.0

Rectangle {
    id: fileBrowser
    objectName: "file_browser"
    anchors.fill: parent
    visible: false
    z: 2

    // header, footer need this
    property Rectangle header
    property TextField path

    function urlToString(url) { return url.toString().substring("file://".length) }

    ListView {
        id: folderView
        objectName: "folder_view"
        anchors.fill: parent
        delegate: Ext.FileDelegate {}
        currentIndex: -1 // no initial highlight
        headerPositioning: ListView.OverlayHeader
        footerPositioning: ListView.OverlayHeader

        property var colors: ["white", "#f0f0f0"]

        model: FolderListModel {
            id: folderModel
            objectName: "folder_model"
            showDirsFirst: true
            showHidden: true
            nameFilters: ["*.lisp", "*.lsp", "*.asd", "*.fas", "*.fasb", "*.fasc", ".eclrc"]
        }

        header: Rectangle {
            id: header
            width: fileBrowser.width
            height: headerColumn.height
            z: 2
            color: "#f0f0f0"

            Component.onCompleted: fileBrowser.header = header // header, footer need this

            Column {
                id: headerColumn

                Row {
                    id: buttonRow
                    spacing: 4

                    // one directory up
                    Ext.DialogButton {
                        text: "\uf062"
                        onClicked: Lisp.call("dialogs:set-file-browser-path", urlToString(folderModel.parentFolder))
                    }

                    // storage
                    Ext.DialogButton {
                        text: "\uf0c7"
                        onClicked: Lisp.call("dialogs:set-file-browser-path", ":storage")
                    }

                    // documents
                    Ext.DialogButton {
                        text: "\uf0f6"
                        onClicked: Lisp.call("dialogs:set-file-browser-path", ":documents")
                    }

                    // home
                    Ext.DialogButton {
                        text: "\uf015"
                        onClicked: Lisp.call("dialogs:set-file-browser-path", ":home")
                    }
                }

                TextField {
                    id: path
                    objectName: "path"
                    width: fileBrowser.width
                    inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
                    text: urlToString(folderModel.folder)

                    Component.onCompleted: fileBrowser.path = path // header, footer need this

                    onAccepted: Lisp.call("dialogs:set-file-name", text)
                }
            }

            // back
            Ext.DialogButton {
                x: header.width - width
                text: "\uf105"

                onClicked: {
                    fileBrowser.visible = false
                    Lisp.call("dialogs:set-file-name", "")
                }
            }
        }

        Row {
            y: header.height + 5
            anchors.horizontalCenter: parent.horizontalCenter
            spacing: 15
            visible: path.focus

            // cursor back
            Ext.ArrowButton {
                opacity: 0.1
                text: "\uf137"

                onPressed:      path.cursorPosition--
                onPressAndHold: path.cursorPosition = 0
            }

            // cursor forward
            Ext.ArrowButton {
                opacity: 0.1
                text: "\uf138"

                onPressed:      path.cursorPosition++
                onPressAndHold: path.cursorPosition = path.length
            }
        }

        footer: Rectangle {
            width: fileBrowser.width
            height: itemCount.height + 4
            z: 2
            color: "lightgray"
            border.width: 1
            border.color: "gray"

            Row {
                anchors.fill: parent

                Text {
                    id: itemCount
                    text: Lisp.call("format", null, " ~D item~P", folderModel.count, folderModel.count)
                    anchors.verticalCenter: parent.verticalCenter
                }
            }
        }
    }
}
