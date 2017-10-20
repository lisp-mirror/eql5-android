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

    function urlToString(url) {
        return url.toString().substring("file://".length) }

    ListView {
        id: folderView
        objectName: "folder_view"
        anchors.fill: parent
        delegate: Ext.FileDelegate {}
        headerPositioning: ListView.OverlayHeader
        footerPositioning: ListView.OverlayHeader

        property var colors: ["white", "#f0f0f0"]

        model: FolderListModel {
            id: folderModel
            objectName: "folder_model"
            showDirsFirst: true
            nameFilters: ["*.lisp", "*.lsp", "*.fas", ".fasb", ".fasc"]
        }

        header: Rectangle {
            id: header
            width: fileBrowser.width
            height: headerColumn.height
            z: 2
            border.width: 1
            border.color: "lightgray"

            Column {
                id: headerColumn

                TextField {
                    id: path
                    width: fileBrowser.width
                    text: urlToString(folderModel.folder)

                    onAccepted: {
                        fileBrowser.visible = false
                        Lisp.call("dialogs:set-file-name", text)
                    }
                }

                Row {
                    id: buttonRow
                    spacing: 4

                    Button {
                        id: folderUp
                        width: 1.2 * height
                        font.family: fontAwesome.name
                        font.pixelSize: 24
                        text: "\uf062"

                        onClicked: Lisp.call("dialogs:set-file-browser-path", urlToString(folderModel.parentFolder))
                    }
                    Button {
                        width: 1.2 * height
                        font.family: fontAwesome.name
                        font.pixelSize: 24
                        text: "\uf015"

                        onClicked: Lisp.call("dialogs:set-file-browser-path", ":home")
                    }
                    Button {
                        width: 1.2 * height
                        font.family: fontAwesome.name
                        font.pixelSize: 20
                        text: "\uf15b"

                        onClicked: Lisp.call("dialogs:set-file-browser-path", ":documents")
                    }
                }
            }

            Button {
                x: header.width - width
                y: buttonRow.y
                text: "Cancel"

                onClicked: {
                    fileBrowser.visible = false
                    Lisp.call("dialogs:set-file-name", "")
                }
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
                    text: " " + folderModel.count + " items"
                    anchors.verticalCenter: parent.verticalCenter
                }
            }
        }
    }
}
