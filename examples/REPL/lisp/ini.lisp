(in-package :eql-user)

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
    (qlater (lambda () (editor::eval* "(post-install)")))))

;; Quicklisp setup (stolen from 'ecl-android')

(defun quicklisp ()
  (flet ((sym (sym pkg)
           (intern (symbol-name sym) pkg)))
    (require :ecl-quicklisp)
    (require :deflate)
    (require :ql-minitar)
    ;; replace interpreted function with precompiled one from DEFLATE
    (setf (symbol-function (sym 'gunzip :ql-gunzipper))
          (symbol-function (sym 'gunzip :deflate)))))

;; Swank setup (stolen from 'ecl-android')

;; TODO

#|
(format t "Preparing swank~%")
(ql:quickload 'swank :verbose t)
(swank-loader:init :load-contribs t :setup t :delete t :quiet t)

(in-package :swank/backend)
(defimplementation lisp-implementation-program ()
  "Return the argv[0] of the running Lisp process, or NIL."
  "org.lisp.ecl")

(in-package :cl-user)
(defun start-swank ()
  (format t "Starting swank server~%")
  (mp:process-run-function
   "SLIME-listener"
   (lambda ()
     (let ((swank::*loopback-interface* "0.0.0.0"))
       (swank:create-server :port 4005
                            :dont-close t
                            ;; :style nil #|:spawn|#
                            )))))

(defun stop-swank ()
  (format t "Stopping swank server~%")
  (swank:stop-server 4005)
  (format t ";; Swank off-line~%"))
|#
