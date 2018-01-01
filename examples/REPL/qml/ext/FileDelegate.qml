import QtQuick 2.7
import EQL5 1.0

Rectangle {
    width: folderView.width
    height: 48
    color: (index == folderView.currentIndex) ? "lightblue" : folderView.colors[index & 1]

    Row {
        anchors.fill: parent

        Text {
            id: icon
            width: 38
            anchors.verticalCenter: parent.verticalCenter
            font.family: fontAwesome.name
            font.pixelSize: 24
            text: fileIsDir ? " \uf115" : " \uf016"
        }
        Text {
            width: 3/4 * folderView.width - icon.width
            anchors.verticalCenter: parent.verticalCenter
            text: fileName
        }
        Text {
            width: 1/4 * folderView.width - 4
            anchors.verticalCenter: parent.verticalCenter
            horizontalAlignment: Text.AlignRight
            text: fileIsDir ? "" : fileSize
        }
    }

    MouseArea {
        anchors.fill: parent

        onClicked: {
            // highlight selected
            folderView.currentIndex = index
            Lisp.call("qsleep", 0.15)
            folderView.currentIndex = -1

            if(fileIsDir) {
                Lisp.call("dialogs:set-file-browser-path", filePath)
            }
            else {
                fileBrowser.visible = false
                Lisp.call("dialogs:set-file-name", filePath)
            }
        }
    }
}
