(load "dependencies")
(load "lisp/qml-lisp")
(load "package")
(load "lisp/ini")
(load "lisp/data/lisp-keywords")
(load "lisp/data/eql-keywords")
(load "lisp/data/keywords")
(load "lisp/input-hook")
(load "lisp/top-level")
(load "lisp/eval")
(load "lisp/dialogs")
(load "lisp/thread-safe")
(load "lisp/editor")

(progn
  (editor:start)
  (|showMaximized| qml:*quick-view*))
