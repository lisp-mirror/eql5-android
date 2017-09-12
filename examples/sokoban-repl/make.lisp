(load "../../utils/EQL5-symbols")
(load "../../utils/cross-compile")

(require :cmp)

(setf *break-on-signals* 'error)

(in-package :eql-user)

(defparameter *files* '("lisp/3rd-party/sokoban"
                        "lisp/3rd-party/my-levels"
                        "lisp/qml-lisp"
                        "lisp/eval"
                        "lisp/thread-safe"
                        "lisp/sokoban"
                        "lisp/ini"))

(setf *load-verbose* nil)
(setf *compile-verbose* t)
(setf c::*suppress-compiler-warnings* nil)
(setf c::*suppress-compiler-notes* nil)

(setf c::*compile-in-constants* t)

(trace c::builder) ; print out all the object files involved

(push :release *features*)

(defparameter *force-compile* (find "-f" (ext:command-args) :test 'string=))

(dolist (file *files*)
  (let ((src (x:cc file ".lisp"))
        (obj (x:cc file ".o")))
    (print src)
    ;; exclude files using inline C code
    (unless (find (pathname-name src) '(#| nothing yet |#) :test 'string=)
      (load src))
    (when (or *force-compile*
              (> (file-write-date src)
                 (if (probe-file obj)
                     (file-write-date obj)
                     0)))
      (cross:compile-file* file))))

(cross:build-static-library* "build/app"
                             :lisp-files (mapcar (lambda (file) (x:cc file ".o"))
                                                 *files*)
                             :init-name "ini_app"
                             :epilogue-code '(qsoko:start))
