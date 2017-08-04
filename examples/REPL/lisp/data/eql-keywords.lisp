(in-package :editor)

(defvar *eql-keywords*
  '("define-qt-wrappers"
    "defvar-ui"
    "ensure-qt-object"
    "in-home"
    "new-qt-object"
    "qadd-event-filter"
    "qapp"
    "qapropos"
    "qapropos*"
    "qauto-reload-c++"
    "qcall-default"
    "qclear-event-filters"
    "qconnect"
    "qcopy"
    "qdel"
    "qdelete"
    "qdisconnect"
    "qenums"
    "qeql"
    "qescape"
    "qevents"
    "qexec"
    "qexit"
    "qfind-bound"
    "qfind-bound*"
    "qfind-child"
    "qfind-children"
    "qfrom-utf8"
    "qfun"
    "qfun*"
    "qfun+"
    "qfuns"
    "qget"
    "qgui"
    "qid"
    "qinvoke-method"
    "qinvoke-method*"
    "qinvoke-method+"
    "qinvoke-methods"
    "qlater"
    "qlet"
    "qload"
    "qload-c++"
    "qload-ui"
    "qlocal8bit"
    "qmessage-box"
    "qmsg"
    "qnew"
    "qnew-instance"
    "qnew*"
    "qnew-instance*"
    "qnull"
    "qnull-object"
    "qobject-names"
    "qok"
    "qoverride"
    "qprocess-events"
    "qproperties"
    "qproperties*"
    "qproperty"
    "qq"
    "qquit"
    "qremove-event-filter"
    "qrequire"
    "qrgb"
    "qrun"
    "qrun-in-gui-thread"
    "qrun*"
    "qrun-in-gui-thread*"
    "qsel"
    "qselect"
    "qsender"
    "qset"
    "qset-color"
    "qset-ini"
    "qset-null"
    "qset-property"
    "qsignal"
    "qsingle-shot"
    "qsleep"
    "qslot"
    "qstatic-meta-object"
    "qsuper-class-name"
    "qt-object"
    "qt-object-id"
    "qt-object-name"
    "qt-object-p"
    "qt-object-pointer"
    "qt-object-unique"
    "qt-object-?"
    "qtranslate"
    "quic"
    "qui-class"
    "qui-names"
    "qutf8"
    "qvariant-from-value"
    "qvariant-value"
    "qversion"
    "tr"
    ;; QML
    "qml-get"
    "qml-set"
    "qml-set-all"
    "qml-call"
    "find-quick-item"
    "js"
    ))