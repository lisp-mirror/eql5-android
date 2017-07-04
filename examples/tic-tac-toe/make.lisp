(load "../../utils/EQL5-symbols")
(load "build/cross-compile")

(require :cmp)

(setf *break-on-signals* 'error)

(in-package :eql-user)

(defparameter *files* '("lisp/qml-lisp"
                        "lisp/game-logic"
                        "lisp/tic-tac-toe"))

(setf *load-verbose* nil)
(setf *compile-verbose* t)
(setf c::*suppress-compiler-warnings* nil)
(setf c::*suppress-compiler-notes* nil)

(setf c::*compile-in-constants* t)

(trace c::builder)

(push :release *features*)

(dolist (file *files*)
  (cross:compile-file* file))

(cross:build-static-library* "build/app"
                             (mapcar (lambda (file) (x:cc file ".o"))
                                             *files*))

(dolist (file *files*)
  (delete-file (x:cc file ".o")))

