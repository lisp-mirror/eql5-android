;;;
;;; This assumes that you already ran 'make-0-ASDF.lisp', in order to build
;;; the whole list of all files of your 'repl.asd' system.
;;; 
;;; (You only need to repeat the above on Quicklisp/ASDF dependency changes.)
;;;
;;; Being the whole file list already generated in 'files.txt', cross-compiling
;;; is now a trivial task!
;;;

(load "../../utils/EQL5-symbols")
(load "../../utils/cross-compile")

(require :cmp)

(setf *break-on-signals* 'error)

(setf *load-verbose* nil)
(setf *compile-verbose* t)
(setf c::*suppress-compiler-warnings* nil)
(setf c::*suppress-compiler-notes* nil)

(setf c::*compile-in-constants* t)

(trace c::builder) ; print out all the object files involved

(push :release *features*)

(defparameter *files* (with-open-file (s "files.txt" :direction :input)
                        (loop :for line = (read-line s nil nil)
                              :while line :collect line)))

(dolist (file *files*)
  (let ((src (x:cc file ".lisp"))
        (obj (x:cc file ".o")))
    (print src)
    ;; exclude files using inline C code
    (unless (find (pathname-name src) '(#| nothing yet |#) :test 'string=)
      (load src))
    (when (> (file-write-date src)
             (if (probe-file obj)
                 (file-write-date obj)
                 0))
      (cross:compile-file* file))))

(cross:build-static-library* "build/app"
                             :lisp-files (mapcar (lambda (file) (x:cc file ".o"))
                                                 *files*)
                             :init-name "ini_app"
                             :epilogue-code '(editor:start))

