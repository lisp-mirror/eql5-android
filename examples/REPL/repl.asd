(defsystem :repl
  :serial t
  ;; these dependencies are not required for the REPL, they are
  ;; just here as examples for integrating Quicklisp libraries
  :depends-on (:split-sequence
               :cl-ppcre
               :iterate)
  :components ((:file "lisp/qml-lisp")
               (:file "package")
               (:file "lisp/ini")
               (:file "lisp/data/lisp-keywords")
               (:file "lisp/data/eql-keywords")
               (:file "lisp/data/keywords")
               (:file "lisp/input-hook")
               (:file "lisp/top-level")
               (:file "lisp/eval")
               (:file "lisp/dialogs")
               (:file "lisp/thread-safe")
               (:file "lisp/editor")))
