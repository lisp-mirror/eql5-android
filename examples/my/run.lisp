(load "lisp/qml-lisp")
(load "lisp/eval")
(load "lisp/thread-safe")
(load "lisp/my")
(load "lisp/dialogs")
(load "lisp/ini")

(progn
  (my:start)
  (|showMaximized| qml:*quick-view*)
  (qml:qml-set "repl_input" "font.family" "Monospace"))
