#-eql5
(error "Please use the EQL5 executable to run this file")

(require :cmp)

(in-package :eql-user)

(defparameter *files* '("lisp/qml-lisp"
                        "lisp/game-logic"
                        "lisp/tic-tac-toe"))

(setf *load-verbose* nil)
(setf *compile-verbose* nil)
(setf c::*suppress-compiler-warnings* t)
(setf c::*suppress-compiler-notes* t)

(setf c::*compile-in-constants* t)

(trace c::builder)

(push :release *features*)

(dolist (file *files*)
  (compile-file file :system-p t))

(c:build-static-library "build/app_desktop"
                        :lisp-files (mapcar (lambda (file) (x:cc file ".o"))
                                            *files*)
                        :init-name "ini_app")

(dolist (file *files*)
  (delete-file (x:cc file ".o")))

(qq)

