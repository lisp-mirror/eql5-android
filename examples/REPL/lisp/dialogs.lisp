(defpackage :dialogs
  (:use :cl :eql :qml)
  (:export
   #:query-dialog
   #:debug-dialog
   #:get-file-name
   #:exited))

(in-package :dialogs)

(defvar *file-dialog-component* nil)
(defvar *file-dialog-instance*  nil)

(defun query-dialog (query)
  (unless (x:empty-string query)
    (qml-set "query_text" "text" query))
  (qml-call "query_input" "clear")
  (qml-call "query_dialog" "open")
  (wait-for-closed)
  (qml-get "query_input" "text"))

(defun debug-dialog (messages)
  (qml-call "debug_text" "clear")
  (dolist (text/color messages)
    (qml-call "debug_text" "append"
              (format nil "<pre><font face='Droid Sans Mono' color='~A'>~A</font></pre>"
                      (cdr text/color)
                      (x:string-substitute "<br>" (string #\Newline) (qescape (car text/color))))))
  (qml-call "debug_dialog" "open")
  (wait-for-closed)
  (qml-get "debug_input" "text"))

(defun qml-component (file)
  (qnew "QQmlComponent(QQmlEngine*,QUrl)"
        (|engine| *quick-view*)
        (qml:file-to-url file)))

(defun get-file-name (callback)
  (unless *file-dialog-component*
    (setf *file-dialog-component* (qml-component "qml/ext/FileDialog.qml")))
  ;; new instance on every call to ensure correct size depending on landscape/portrait
  (when *file-dialog-instance*
    (qdel *file-dialog-instance*))
  (setf *file-dialog-instance* (qt-object-? (|create| *file-dialog-component*)))
  (|setParent| *file-dialog-instance* (qml:root-item))
  (qml-set "file_dialog" "callback" (prin1-to-string callback))
  (qml-call "file_dialog" "open"))

(defun wait-for-closed ()
  (mp:process-suspend eval:*eval-thread*))

(defun exited () ; called from QML
  (mp:process-resume eval:*eval-thread*))

