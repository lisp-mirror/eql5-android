;;;
;;; This is only a dummy build in order to collect all file names in the
;;; right order, and save the file list for cross-compiling.
;;;
;;; Before running this file, you need to delete 2 directories.
;;;
;;; Change to (the equivalent in your home dir):
;;;
;;;   ~/.cache/common-lisp/ecl-16.1.3-unknown-linux-x64/home/username
;;;
;;; Now delete these 2 dirs:
;;;
;;;   eql5-android/examples/REPL
;;;   quicklisp/dists/quicklisp/software
;;;
;;; That means deleting all compiled Quicklisp files (because of eventual
;;; recursive dependencies).
;;;
;;; ----------------------------------------------------------------------
;;;
;;; THIS IS A HACK (of course), but it seems to be the LEAST CUMBERSOME of
;;; all the other variants of cross-compiling Quicklisp systems to android.
;;;
;;; Remember that you only need to do this once (needs to be repeated only
;;; on changes of your Quicklisp/ASDF dependencies).
;;;
;;; Remember also to add eventual new source files manually to "files.txt",
;;; in order to avoid running this file again.
;;;

(require :cmp)

(ext:package-lock :common-lisp nil)

(defvar *compile-file-orig* (symbol-function 'compile-file))
(defvar *files*             nil)

(defun compile-file (&rest args)
  (let* ((str (namestring (first args)))
         (name (subseq str 0 (position #\. str :from-end t))))
    (push (print name) *files*))
  (apply *compile-file-orig* args))

(setf c::*compile-in-constants* t)

(push "./" asdf:*central-registry*)

;;; load here (not earlier)

(load "../../utils/EQL5-symbols")
(load "dependencies")

;; dummy build to collect all file names

(asdf:make-build "repl"
                 :monolithic t
                 :type :fasl
                 :move-here "./tmp/")

;; save file names for cross-compile build

(with-open-file (s "files.txt" :direction :output :if-exists :supersede)
  (dolist (file (nreverse *files*))
    (format s "~A~%" file)))

