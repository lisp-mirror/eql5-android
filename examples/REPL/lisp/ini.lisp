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

(defun sym (symbol package)
  (intern (symbol-name symbol) package))

(defun quicklisp ()
  (unless (find-package :quicklisp)
    (require :ecl-quicklisp)
    (require :deflate)
    (require :ql-minitar)
    ;; replace interpreted function with precompiled one from DEFLATE
    (setf (symbol-function (sym 'gunzip :ql-gunzipper))
          (symbol-function (sym 'gunzip :deflate)))))

;; Swank setup (stolen from 'ecl-android')

(defun start-swank (&key (load-contribs t) (setup t) (delete t) (quiet t))
  (quicklisp)
  (funcall (sym 'quickload :ql) :swank :verbose t)
  (funcall (sym 'init :swank-loader)
           :load-contribs load-contribs
           :setup setup
           :delete delete
           :quiet quiet)
  (eval (read-from-string "(swank/backend:defimplementation swank/backend:lisp-implementation-program () \"org.lisp.ecl\")"))
  ;; QRUN*: use main thread to start new thread
  (qrun* (mp:process-run-function
          "SLIME-listener"
          (lambda ()
            (setf (symbol-value (sym '*loopback-interface* :swank)) "0.0.0.0")
            (funcall (sym 'create-server :swank)
                     :port 4005
                     :dont-close t)))))

(defun stop-swank ()
  (funcall (sym 'stop-server :swank) 4005)
  :stopped)

