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

(let ((ini ".eql5-repl-ini"))
  (defun post-install ()
    (when (copy-asset-files *assets-lib*)
      (with-open-file (s ini :direction :output)
        (write-string "ok" s)
        :done)))
  #+release
  (unless (probe-file ini)
    (qlater (lambda () (editor::eval* "(eql-user::post-install)")))))

;; Quicklisp setup

;; TODO

