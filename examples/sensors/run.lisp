(load "lisp/qml-lisp")
(load "lisp/eval")
(load "lisp/dialogs")
(load "lisp/thread-safe")
(load "package")
(load "lisp/ini")
(load "lisp/sensors-ini")
(load "lisp/sensors")

(progn
  (sensors:start)
  (|showMaximized| qml:*quick-view*)
  (qml:qml-set "repl_input" "font.family" "Monospace"))
