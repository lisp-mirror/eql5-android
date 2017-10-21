(defpackage :dialogs
  (:use :cl :eql :qml)
  (:export
   #:query-dialog
   #:debug-dialog
   #:get-file-name
   #:exited
   #:*file-name*))

(in-package :dialogs)

(defvar *file-name*        nil)
(defvar *callback*         nil)
(defvar *suspended-thread* nil)

(defvar *qml-query-dialog* "query_dialog")
(defvar *qml-query-text*   "query_text")
(defvar *qml-query-input*  "query_input")
(defvar *qml-debug-dialog* "debug_dialog")
(defvar *qml-debug-text*   "debug_text")
(defvar *qml-debug-input*  "debug_input")
(defvar *qml-file-browser* "file_browser")
(defvar *qml-folder-model* "folder_model")

(defun query-dialog (query)
  (unless (x:empty-string query)
    (qml-set *qml-query-text* "text" (string-trim '(#\Newline) query)))
  (qml-call *qml-query-input* "clear")
  (qml-set *qml-query-dialog* "visible" t)
  (qml-call *qml-query-input* "forceActiveFocus")
  (wait-for-closed)
  (qml-set *qml-query-dialog* "visible" nil)
  (qml-get *qml-query-input* "text"))

(defun debug-dialog (messages)
  (qml-call *qml-debug-text* "clear")
  (dolist (text/color messages)
    (qml-call *qml-debug-text* "append"
              (format nil "<pre><font face='Droid Sans Mono' color='~A'>~A</font></pre>"
                      (cdr text/color)
                      (x:string-substitute "<br>" (string #\Newline) (qescape (car text/color))))))
  (qml-set *qml-debug-dialog* "visible" t)
  (qml-call *qml-debug-input* "forceActiveFocus")
  (wait-for-closed)
  (qml-set *qml-debug-dialog* "visible" nil)
  (qml-get *qml-debug-input* "text"))

(defun wait-for-closed ()
  (unless (eql (mp:process-name mp:*current-process*)
               'si:top-level)
    (setf *suspended-thread* mp:*current-process*)
    (mp:process-suspend mp:*current-process*)))

(defun exited () ; called from QML
  (when *suspended-thread*
    (mp:process-resume *suspended-thread*)))

;; file browser

(let ((1st t))
  (defun get-file-name (&optional callback)
    (|hide| (|inputMethod.QGuiApplication|))
    (when 1st
      (setf 1st nil)
      (set-file-browser-path ":documents"))
    (setf *callback* callback)
    (qml-set *qml-file-browser* "visible" t)))

(defun set-file-name (name) ; called from QML
  (setf *file-name* name)
  (when *callback*
    (funcall *callback*)))

(defun location (name)
  (first (|standardLocations.QStandardPaths|
          (cond ((string= ":home" name)
                 |QStandardPaths.HomeLocation|)
                ((string= ":documents" name)
                 |QStandardPaths.DocumentsLocation|)))))

(defun set-file-browser-path (path) ; called from QML
  (qlet ((url "QUrl(QString)"
              (x:cc "file://" (if (x:starts-with ":" path)
                                  (location path)
                                  path))))
    (qml-set *qml-folder-model* "folder" url)))
