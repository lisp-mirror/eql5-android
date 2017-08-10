import QtQuick 2.3
import QtQuick.Dialogs 1.2
import EQL5 1.0

FileDialog {
    objectName: "file_dialog"

    title: "Please choose a file"
    nameFilters: [ "Lisp files (*.lisp *.lsp)" ]
    folder: shortcuts.home

    property string callback

    onAccepted: Lisp.call(callback, Qt.resolvedUrl(fileUrl).toString())
}
