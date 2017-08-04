import QtQuick 2.3
import QtQuick.Dialogs 1.2
import EQL5 1.0

FileDialog {
    title: "Please choose a file"
    nameFilters: [ "Lisp files (*.lisp *.lsp)" ]
    folder: shortcuts.home

    property string file

    function exit() { Lisp.call("dialogs:exit-event-loop") }

    onAccepted: { file = fileUrl; exit() }
    onRejected: { file = ""; exit() }
}
