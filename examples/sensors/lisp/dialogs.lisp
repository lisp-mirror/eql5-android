(defpackage :dialogs
  (:use :cl :eql :qml)
  (:export
   #:load-file
   #:get-file-name
   #:location
   #:*file-name*))

(in-package :dialogs)

(defvar *file-name* nil)
(defvar *callback*  nil)

(defvar *qml-main*         "main")         ; StackView
(defvar *qml-file-browser* "file_browser")
(defvar *qml-folder-model* "folder_model")

;; '(js *qml-main* ...)': see JS functions in '../qml/*.qml'

(defun push-dialog (name)
  (js *qml-main* "push~ADialog()" (string-capitalize name)))

(defun pop-dialog ()
  "Pops the currently shown dialog, returning T if there was a dialog to pop."
  (prog1
      (> (qml-get *qml-main* "depth") 1)
    (js *qml-main* "popDialog()")
    (exited)))

(defun wait-while-transition ()
  ;; needed for evtl. recursive calls
  (x:while (qml-get *qml-main* "busy")
    (qsleep 0.1)))

(let ((exited t))
  (defun wait-for-closed ()
    (setf exited nil)
    (x:while (not  exited)
      (qsleep 0.1)))
  (defun exited () ; called from QML
    (setf exited t)))

;; file browser

(let ((1st t))
  (defun get-file-name (&optional callback)
    #+android
    (ensure-android-permission) ; defaults to 'external storage'
    (|hide| (|inputMethod.QGuiApplication|))
    (when 1st
      (setf 1st nil)
      (set-file-browser-path ":data"))
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

(defun load-file ()
  (get-file-name 'do-load-file))

(defun do-load-file ()
  (unless (x:empty-string *file-name*)
    (if (probe-file *file-name*)
        (let ((type (pathname-type *file-name*)))
          (when (or (x:starts-with "fas" type)
                    (find type '("lisp" "lsp") :test 'string=))
            (eval::append-output (prin1-to-string (load *file-name*))
                                 eval::*color-values*)))
        (qmsg (format nil "File does not exist:~%~%~S" *file-name*)))))

(defun location (name)
  (cond ((string= ":storage" name)
         #+android "/storage" #-android "/")
        ((string= ":data" name)
         (first (|standardLocations.QStandardPaths| |QStandardPaths.GenericDataLocation|)))
        ((string= ":home" name)
         (namestring *default-pathname-defaults*))))

(defun set-file-browser-path (path) ; called from QML
  (qlet ((url "QUrl(QString)"
              (x:cc "file://" (if (x:starts-with ":" path)
                                  (location path)
                                  path))))
    (qml-set *qml-folder-model* "folder" url)))
