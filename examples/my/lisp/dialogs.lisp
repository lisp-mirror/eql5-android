(defpackage :dialogs
  (:use :cl :eql :qml)
  (:export
   #:load-file
   #:get-file-name
   #:*file-name*
   #:*qml-file-dialog*))

(in-package :dialogs)

(defvar *file-name*             nil)
(defvar *file-dialog-component* nil)
(defvar *file-dialog-instance*  nil)

(defvar *qml-file-dialog*       "file_dialog")

(defun qml-component (file)
  (qnew "QQmlComponent(QQmlEngine*,QUrl)"
        (|engine| *quick-view*)
        (qml:file-to-url file)))

(defun get-file-name (&key callback save)
  (unless *file-dialog-component*
    (setf *file-dialog-component* (qml-component "qml/ext/FileDialog.qml")))
  ;; new instance on every call to ensure correct size depending on landscape/portrait
  (when *file-dialog-instance*
    (qdel *file-dialog-instance*))
  (setf *file-dialog-instance* (qt-object-? (|create| *file-dialog-component*)))
  (|setParent| *file-dialog-instance* (qml:root-item))
  (when callback
    (qml-set *qml-file-dialog* "callback" (prin1-to-string callback)))
  (qml-set *qml-file-dialog* "selectExisting" (not save))
  (qml-call *qml-file-dialog* "open"))

(defun set-file-name (name) ; called from QML
  (setf *file-name* name))

(defun load-file ()
  (get-file-name :callback 'do-load-file))

(defun do-load-file (&optional (name *file-name*)) ; called from QML
  (unless (x:empty-string name)
    (let ((type (pathname-type name)))
      (when (or (x:starts-with "fas" type)
                (find type '("lisp" "lsp") :test 'string=))
        (eval::append-output (prin1-to-string (load name))
                             eval::*color-values*)))))
