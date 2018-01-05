(defpackage :dialogs
  (:use :cl :eql :qml)
  (:export
   #:query-dialog
   #:debug-dialog
   #:get-file-name
   #:exited
   #:push-dialog
   #:pop-dialog
   #:*file-name*))

(in-package :dialogs)

(defvar *file-name*        nil)
(defvar *callback*         nil)
(defvar *suspended-thread* nil)

(defvar *qml-main*         "main")         ; StackView
(defvar *qml-query-text*   "query_text")
(defvar *qml-query-input*  "query_input")
(defvar *qml-debug-text*   "debug_text")
(defvar *qml-debug-input*  "debug_input")
(defvar *qml-folder-model* "folder_model")

;; '(js *qml-main* ...)': see JS functions in '../qml/repl.qml'

(defun push-dialog (name)
  (js *qml-main* "push~ADialog()" (string-capitalize name)))

(defun pop-dialog ()
  "Pops the currently shown dialog, returning T if there was a dialog to pop."
  (prog1
      (> (qml-get *qml-main* "depth") 1)
    (js *qml-main* "popDialog()")
    (exited))) ; needed in some cases (eval thread)

(defun wait-while-transition ()
  ;; needed for evtl. recursive calls
  (x:while (qml-get *qml-main* "busy")
    (qsleep 0.05)))

(defun query-dialog (query)
  (unless (x:empty-string query)
    (qml-set *qml-query-text* "text" (string-trim '(#\Newline) query)))
  (qml-call *qml-query-input* "clear")
  (wait-while-transition)
  (push-dialog :query)
  (qml-call *qml-query-input* "forceActiveFocus")
  (|show| (|inputMethod.QGuiApplication|)) ; needed on recursive calls
  (wait-for-closed)
  (pop-dialog)
  (qml-get *qml-query-input* "text"))

(defun debug-dialog (messages)
  (qml-call *qml-debug-text* "clear")
  (dolist (text/color messages)
    (qml-call *qml-debug-text* "append"
              (format nil "<pre><font face='Droid Sans Mono' color='~A'>~A</font></pre>"
                      (cdr text/color)
                      (x:string-substitute "<br>" (string #\Newline)
                                           (qescape (string-trim '(#\Newline) (car text/color)))))))
  (wait-while-transition)
  (push-dialog :debug)
  (qml-call *qml-debug-input* "forceActiveFocus")
  (wait-for-closed)
  (pop-dialog)
  (qlater (lambda () (editor:ensure-focus :show)))
  (qml-get *qml-debug-input* "text"))

(defun wait-for-closed ()
  (unless (eql (mp:process-name mp:*current-process*)
               'si:top-level)
    (setf *suspended-thread* mp:*current-process*)
    (mp:process-suspend mp:*current-process*)))

(defun exited () ; called from QML
  (when *suspended-thread*
    (mp:process-resume *suspended-thread*)
    (setf *suspended-thread* nil)))

;; file browser

(let ((1st t))
  (defun get-file-name (&optional callback)
    (|hide| (|inputMethod.QGuiApplication|))
    (when 1st
      (setf 1st nil)
      (set-file-browser-path ":documents"))
    (setf *callback* callback)
    ;; force update
    (qlet ((none "QUrl")
           (curr (qml-get *qml-folder-model* "folder")))
      (dolist (folder (list none curr))
        (qml-set *qml-folder-model* "folder" folder)))
    (push-dialog :file)))

(defun directory-p (path)
  (qlet ((info "QFileInfo(QString)" path))
    (|isDir| info)))

(defun set-file-name (file-name) ; called from QML
  (let ((name (remove-if (lambda (ch) (find ch "*?\\")) file-name)))
    (if (directory-p name)
        (set-file-browser-path name)
        (progn
          (pop-dialog)
          (setf *file-name* name)
          (when *callback*
            (funcall *callback*))))))

(defun location (name)
  (if (string= ":storage" name)
      #+android "/storage" #-android "/"
      (first (|standardLocations.QStandardPaths|
              (cond ((string= ":home" name)
                     |QStandardPaths.HomeLocation|)
                    ((string= ":documents" name)
                     |QStandardPaths.DocumentsLocation|))))))

(defun set-file-browser-path (path) ; called from QML
  (qlet ((url "QUrl(QString)"
              (x:cc "file://" (if (x:starts-with ":" path)
                                  (location path)
                                  path))))
    (qml-set *qml-folder-model* "folder" url)))
