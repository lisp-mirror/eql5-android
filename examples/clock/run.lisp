(load "lisp/qml-lisp")
(load "package.lisp")
(load "lisp/clock.lisp")
(load "lisp/painted-item.lisp")

(progn
  (painted-item:start)
  (|show| qml:*quick-view*))
