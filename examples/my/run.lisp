(load "lisp/qml-lisp")
(load "lisp/eval")
(load "lisp/dialogs")
(load "lisp/thread-safe")
(load "package")
(load "lisp/ini")
(load "lisp/my-ini")
(load "lisp/my")

(progn
  (my:start)
  (|showMaximized| qml:*quick-view*)
  (qml:qml-set "repl_input" "font.family" "Monospace"))
