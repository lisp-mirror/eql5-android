;;;
;;; Includes everything for Quicklisp and Swank.
;;; Requires 'assets/lib/*' to contain all precompiled ECL contribs.
;;;

(in-package :eql)

(use-package :qml)

(defvar *assets-lib* "assets:/lib/")

(defun copy-asset-files (dir-name)
  "Copy all files from APK 'assets/lib/' to home path."
  (flet ((trim (name)
           (if (x:starts-with *assets-lib* name)
               (subseq name (length *assets-lib*))
               name)))
    (qlet ((dir "QDir(QString)" dir-name))
      (ensure-directories-exist (trim (x:cc dir-name "/")))
      (dolist (info (|entryInfoList| dir))
        (if (|isDir| info)
            (copy-asset-files (|filePath| info))
            (let* ((from (|filePath| info))
                   (to (trim from)))
              (unless (or (probe-file to)
                          (|copy.QFile| from to))
                (qmsg (format nil "Error copying asset file: ~S" from))
                (return-from copy-asset-files)))))))
  t)

(defun touch-file (name)
  (open name :direction :probe :if-does-not-exist :create))

(defun post-install ()
  (when (copy-asset-files *assets-lib*)
    (touch-file ".eql5-ini")
    :done))

(defun ini ()
  (si:install-bytecodes-compiler)
  (let ((.eclrc ".eclrc"))
    (if (probe-file .eclrc)
        (load .eclrc)
        (touch-file .eclrc)))
  (unless (probe-file ".eql5-ini")
    (qlater 'post-install)))

;; Quicklisp setup (stolen from 'ecl-android')

(defun sym (symbol package)
  (intern (symbol-name symbol) package))

(defun quicklisp ()
  (unless (find-package :quicklisp)
    (qrun* ; run in main thread (safer on some devices)
     (require :ecl-quicklisp)
     (require :deflate)
     (require :ql-minitar))
    ;; replace interpreted function with precompiled one from DEFLATE
    (setf (symbol-function (sym 'gunzip :ql-gunzipper))
          (symbol-function (sym 'gunzip :deflate))))
  :quicklisp)

(export 'quicklisp)

;; Swank setup (stolen from 'ecl-android')

(defun swank/create-server (port dont-close style)
  (funcall (sym 'create-server :swank)
                :port port
                :dont-close dont-close
                :style style))

(defun start-swank (&key (loopback "0.0.0.0") log-events
                      (load-contribs t) (setup t) (delete t) (quiet t)
                      (port 4005) (dont-close t) style)
  (qrun* ; run in main thread (safer on some devices)
   (unless (find-package :swank)
     (require :asdf)
     (funcall (sym 'load-system :asdf) :swank))
   (funcall (sym 'init :swank-loader)
            :load-contribs load-contribs
            :setup setup
            :delete delete
            :quiet quiet)
   (setf (symbol-value (sym '*loopback-interface* :swank)) loopback)
   (setf (symbol-value (sym '*log-events* :swank)) log-events)
   (eval (read-from-string "(swank/backend:defimplementation swank/backend:lisp-implementation-program () \"org.lisp.ecl\")"))
   (if (eql :spawn style)
       (swank/create-server port dont-close style)
       (mp:process-run-function
        "SLIME-listener"
        (lambda () (swank/create-server port dont-close style))))))

(defun stop-swank ()
  (when (find-package :swank)
    (funcall (sym 'stop-server :swank) 4005)
    :stopped))

(export 'start-swank)
(export 'stop-swank)

;; permissions (android api >= 23)

(defun ensure-android-permission (&optional (name "android.permission.WRITE_EXTERNAL_STORAGE"))
  "Check/request Android permission (for API level >= 23); the name defaults to \"android.permission.WRITE_EXTERNAL_STORAGE\". Returns T on granted permission."
  (! "checkPermission" (:qt (qapp)) name)) ; see ../build/load.h'

(export 'ensure-android-permission)

;; update app

(defvar *qml-folder-model* "folder_model")

(let (name-filters)
  (defun install-update (&optional from)
    "Copies new version of 'libqtapp.so' in 'update/' directory. After restart of the app, the new version will be used."
    (if from
        (do-install-update from)
        (progn
          (unless name-filters
            (setf name-filters (qml-get *qml-folder-model* "nameFilters")))
          (qml-set *qml-folder-model* "nameFilters" (list "*.so"))
          (funcall (sym 'get-file-name :dialogs)
                   (lambda () (do-install-update (symbol-value (sym '*file-name* :dialogs))))))))
  (defun do-install-update (from)
    (unless (x:empty-string from)
      (let ((to "update/libqtapp.so"))
        (ensure-directories-exist to)
        (when (probe-file to)
          (delete-file to))
        (if (|copy.QFile| from to)
            (when (= |QMessageBox.Apply|
                     (|question.QMessageBox| nil "Update" "Update installed successfully.<br><br>Apply update, <b>restarting</b> the app now?"
                                             (logior |QMessageBox.Apply| |QMessageBox.Cancel|)))
              (! "restartApp" (:qt (qapp))) ; see ../build/load.h'
              (qquit))
            (qmsg "<b>Error</b> copying the update."))))
    (qml-set *qml-folder-model* "nameFilters" name-filters))) ; reset (must stay here)

(export 'install-update)

;; shell

(defvar *output* nil)

(export '*output*)

(defun shell (command)
  "Run shell commands; examples:
  (shell \"ls -la\")
  (shell \"ifconfig\")"
  (let ((tmp "tmp.txt"))
    (with-open-file (s tmp :direction :output :if-exists :supersede)
      (ext:run-program "sh" (list "-c" command)
                       :output s)) ; we need a file stream here
    (with-open-file (s tmp)
      (let ((str (make-string (file-length s))))
        (read-sequence str s)
        (fresh-line)
        (princ str)
        (setf *output* (x:split (string-trim '(#\Newline) str) #\Newline))))
    (delete-file tmp))
  (values))

(export 'shell)

;; convenience

(define-symbol-macro :s (start-swank))
(define-symbol-macro :q (quicklisp))
(define-symbol-macro :l (funcall (sym 'load-file :dialogs)))
(define-symbol-macro :f (funcall (sym 'get-file-name :dialogs)))
(define-symbol-macro :r (sensors:reload-qml))

(define-symbol-macro :u (install-update)) ; unofficial

(defun help ()
  (format t "~%~
             ~%  :s  (start-swank)           ; adb forward tcp:4005 tcp:4005~
             ~%  :q  (quicklisp)             ; will install/load it~
             ~%  :l  (dialogs:load-file)     ; load~
             ~%  :f  (dialogs:get-file-name) ; see dialogs:*file-name*~
             ~%  :r  (sensors:reload-qml)    ; see docu~
             ~%~
             ~%  (shell \"ls -la\")            ; see *output*")
  (values))

(export 'help)

;; quit app

(defun back-pressed () ; called from QML
  (qlog :back-pressed)
  (or (funcall (sym 'pop-dialog :dialogs))
      (qquit)))

(export 'back-pressed)

;; ini

(qlater 'ini)
