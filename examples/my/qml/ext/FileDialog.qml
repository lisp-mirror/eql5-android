import QtQuick 2.7
import QtQuick.Dialogs 1.2
import EQL5 1.0

FileDialog {
    objectName: "file_dialog"
    title: "Please choose a file"
    nameFilters: [ "Lisp files (*.lisp *.lsp *.fas*)", "All files (*.*)" ]
    folder: shortcuts.documents

    property string callback

    onAccepted: {
        var name = Qt.resolvedUrl(fileUrl).toString().substring("file://".length)
        Lisp.call("dialogs:set-file-name", name)
        if(callback.length) {
            Lisp.call(callback)
        }
    }
}
