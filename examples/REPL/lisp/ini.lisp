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

(defun android-p ()
  (find :android *features*))

(let ((ini ".eql5-repl-ini"))
  (defun post-install ()
    (when (copy-asset-files *assets-lib*)
      (touch-file ini)
      :done))
  (when (android-p)
    (let ((.eclrc ".eclrc"))
      (if (probe-file .eclrc)
          (load .eclrc)
          (touch-file .eclrc))
      (unless (probe-file ini)
        (qlater (lambda () (editor::eval* "(eql-user::post-install)")))))))

(when (android-p)
  (si:install-bytecodes-compiler))

;; Quicklisp setup

(defun quicklisp ()
  ;; stolen from 'ecl-android'
  (require :ecl-quicklisp)
  (require :deflate)
  (require :ql-minitar)
  ;; replace interpreted function with precompiled one from DEFLATE
  (eval (read-from-string
         "(setf (symbol-function 'ql-gunzipper:gunzip) #'deflate:gunzip)")))

