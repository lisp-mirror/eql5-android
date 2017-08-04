(defpackage :dialogs
  (:use :cl :eql :qml)
  (:export
   #:query-dialog
   #:debug-dialog
   #:get-file-name
   #:exit-event-loop))

(in-package :dialogs)

(defvar *event-loop* (qnew "QEventLoop"))

(defun query-dialog (query)
  (unless (x:empty-string query)
    (qml-set "query_text" "text" query))
  (qml-call "query_input" "clear")
  (qml-call "query_dialog" "open")
  (|exec| *event-loop*)
  (qml-get "query_input" "text"))

(defun debug-dialog (messages)
  (qml-call "debug_text" "clear")
  (dolist (text/color messages)
    (qml-call "debug_text" "append"
              (format nil "<pre><font face='Droid Sans Mono' color='~A'>~A</font></pre>"
                      (cdr text/color)
                      (x:string-substitute "<br>" (string #\Newline) (qescape (car text/color))))))
  (qml-call "debug_dialog" "open")
  (|exec| *event-loop*)
  (qml-get "debug_input" "text"))

(defun get-file-name ()
  (qml-call "file_dialog" "open")
  (|exec| *event-loop*)
  (let ((name (qml-get "file_dialog" "file")))
    (unless (x:empty-string name)
      name)))

(defun exit-event-loop () ; called from QML
  (|exit| *event-loop*))

