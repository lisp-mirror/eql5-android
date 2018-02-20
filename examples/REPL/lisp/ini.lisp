;;;
;;; Includes everything for Quicklisp and Swank.
;;; Requires 'assets/lib/*' to contain all precompiled ECL contribs.
;;;

(in-package :eql-user)

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

(defun delayed-eval (msec string)
  (qsingle-shot msec (lambda () (funcall (sym 'eval* :editor) string))))

(defun post-install ()
  (when (copy-asset-files *assets-lib*)
    (touch-file ".eql5-ini")
    (delayed-eval 1000 "(help t)")
    :done))

(defun ini ()
  (si:install-bytecodes-compiler)
  (if (probe-file ".eclrc")
      (ignore-errors (load ".eclrc")) ; don't hang on startup
      (touch-file ".eclrc"))
  (with-open-file (s ".eclrc" :direction :output :if-exists :append)
    (when (zerop (file-length s))
      (format s ";;; example settings~
               ~%~
               ~%;; bigger font size~
               ~%;;(editor:change-font :bigger 2)~
               ~%~
               ~%;; hide paren buttons~
               ~%;;(qml-set \"rect_paren_buttons\" \"visible\" nil)~
               ~%")))
  (delayed-eval 0 (if (probe-file ".eql5-ini")
                                  "(help t)"
                                  "(post-install)")))

;; Quicklisp setup (stolen from 'ecl-android')

(defun sym (symbol package)
  (intern (symbol-name symbol) package))

(defun eql::quicklisp ()
  (unless (find-package :quicklisp)
    (qrun* ; run in main thread (safer on some devices)
     (require :ecl-quicklisp)
     (require :deflate)
     (require :ql-minitar))
    ;; replace interpreted function with precompiled one from DEFLATE
    (setf (symbol-function (sym 'gunzip :ql-gunzipper))
          (symbol-function (sym 'gunzip :deflate))))
  :quicklisp)

(export 'eql::quicklisp :eql)

;; Swank setup (stolen from 'ecl-android')

(defun swank/create-server (port dont-close style)
  (funcall (sym 'create-server :swank)
                :port port
                :dont-close dont-close
                :style style))

(defun eql::start-swank (&key (loopback "0.0.0.0") log-events
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

(defun eql::stop-swank ()
  (when (find-package :swank)
    (funcall (sym 'stop-server :swank) 4005)
    :stopped))

(export 'eql::start-swank :eql)
(export 'eql::stop-swank  :eql)

;; permissions (android api >= 23)

(defun eql::ensure-android-permission (&optional (name "android.permission.WRITE_EXTERNAL_STORAGE"))
  "Check/request Android permission (for API level >= 23); the name defaults to \"android.permission.WRITE_EXTERNAL_STORAGE\". Returns T on granted permission."
  (! "checkPermission" (:qt (qapp)) name)) ; see ../build/load.h'

(export 'eql::ensure-android-permission :eql)

;; update app

(defvar *qml-folder-model* "folder_model")

(let (name-filters)
  (defun eql::install-update (&optional from)
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

(export 'eql::install-update :eql)

;; shell

(defvar eql::*output* nil)

(export 'eql::*output* :eql)

(defun eql::shell (command)
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

(export 'eql::shell    :eql)

;; convenience

(defvar *qml-output* "output")

(define-symbol-macro :h (help))
(define-symbol-macro :s (start-swank))
(define-symbol-macro :q (quicklisp))
(define-symbol-macro :a (require :asdf))
(define-symbol-macro :f (funcall (sym 'get-file-name :dialogs)))
(define-symbol-macro :c (progn (qml-call *qml-output* "clear") (values)))

(define-symbol-macro :r (funcall (sym 'reload-qml :editor))) ; see README in example 'my'

(define-symbol-macro :u (install-update))    ; unofficial

(defun help (&optional startup)
  (if (and startup (qml-get nil "isPhone"))
      (format t "  :h  (help)")
      (format t "  :s  (start-swank)           ; adb forward tcp:4005 tcp:4005~
               ~%  :q  (quicklisp)             ; will install/load it~
               ~%  :a  (require :asdf)         ; see asdf:load-system~
               ~%  :f  (dialogs:get-file-name) ; see dialogs:*file-name*~
               ~%~
               ~%  (shell \"ls -la\")            ; see *output*~
               ~%~
               ~%  tap and hold to select/copy/paste/eval s-exp (e.g. on 'defun')~
               ~%~
               ~%  tap on [back] to hide keyboard / show cursor move buttons; hold them to move to beginning/end of line/file~
               ~%~
               ~%  double [space] for auto-completion (e.g. m-v-b)"))
  (values))

;; ini

(qlater 'ini)
