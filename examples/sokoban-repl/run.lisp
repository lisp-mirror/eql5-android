(load "lisp/3rd-party/sokoban")
(load "lisp/3rd-party/my-levels")
(load "lisp/qml-lisp")
(load "lisp/eval")
(load "lisp/thread-safe")
(load "lisp/sokoban")
(load "lisp/ini")
(load "lisp/dialogs")

(progn
  (qsoko:start)
  (|showMaximized| qml:*quick-view*)
  (qml:qml-set "repl_input" "font.family" "Monospace"))
