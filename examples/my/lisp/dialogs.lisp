(defpackage :dialogs
  (:use :cl :eql :qml)
  (:export
   #:load-file
   #:get-file-name
   #:location
   #:*file-name*
   #:*qml-file-browser*))

(in-package :dialogs)

(defvar *file-name* nil)
(defvar *callback*  nil)

(defvar *qml-file-browser* "file_browser")
(defvar *qml-folder-model* "folder_model")

(let ((1st t))
  (defun get-file-name (&optional callback)
    (when 1st
      (setf 1st nil)
      (set-file-browser-path ":documents"))
    (setf *callback* callback)
    ;; force update
    (qlet ((none "QUrl")
           (curr (qml-get *qml-folder-model* "folder")))
      (dolist (folder (list none curr))
        (qml-set *qml-folder-model* "folder" folder)))
    (qml-set *qml-file-browser* "visible" t)))

(defun directory-p (path)
  (qlet ((info "QFileInfo(QString)" path))
    (|isDir| info)))

(defun set-file-name (file-name) ; called from QML
  (let ((name (remove-if (lambda (ch) (find ch "*?\\")) file-name)))
    (if (directory-p name)
        (set-file-browser-path name)
        (progn
          (qml-set *qml-file-browser* "visible" nil)
          (|hide| (|inputMethod.QGuiApplication|))
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
