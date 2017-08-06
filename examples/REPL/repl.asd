(defsystem :repl
  :serial t
  :depends-on (:split-sequence
               :cl-ppcre
               :iterate)
  :components ((:file "lisp/qml-lisp")
               (:file "package")
               (:file "lisp/data/lisp-keywords")
               (:file "lisp/data/eql-keywords")
               (:file "lisp/input-hook")
               (:file "lisp/top-level")
               (:file "lisp/eval")
               (:file "lisp/dialogs")
               (:file "lisp/thread-safe")
               (:file "lisp/editor")))
