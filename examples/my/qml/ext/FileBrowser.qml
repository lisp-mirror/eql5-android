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
            color: "#505050"

            Column {
                id: headerColumn

                Row {
                    id: buttonRow
                    spacing: 4

                    // folder up
                    Ext.FileBrowserButton {
                        text: "\uf062"
                        onClicked: Lisp.call("dialogs:set-file-browser-path", urlToString(folderModel.parentFolder))
                    }

                    // documents
                    Ext.FileBrowserButton {
                        text: "\uf15b"
                        onClicked: Lisp.call("dialogs:set-file-browser-path", ":documents")
                    }

                    // home
                    Ext.FileBrowserButton {
                        text: "\uf015"
                        onClicked: Lisp.call("dialogs:set-file-browser-path", ":home")
                    }
                }

                TextField {
                    id: path
                    width: fileBrowser.width
                    inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
                    text: urlToString(folderModel.folder)

                    onAccepted: {
                        fileBrowser.visible = false
                        Lisp.call("dialogs:set-file-name", text)
                    }
                }
            }

            // cancel
            Ext.FileBrowserButton {
                x: header.width - width
                text: "\uf00d"

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
