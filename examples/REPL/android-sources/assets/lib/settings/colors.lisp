(in-package :editor)

(setf *text-color*              "black")
(setf *background-color*        "white")
(setf *parenthesis-color*       "lightslategray")
(setf *string-color*            "saddlebrown")
(setf *comment-color*           "lightslategray")
(setf *lisp-keyword-color*      "#c05050")
(setf *eql-keyword-color*       "#5050c0")
(setf *keyword-color*           "#409090")

(setf *output-text-color*       "black")
(setf *output-background-color* "lavender")
(setf *output-string-color*     "saddlebrown")
(setf *output-value-color*      "#2020ff")
(setf *output-trace-color*      "darkmagenta")

(progn
  (apply-colors)
  (qml-call *qml-output* "clear")
  (values))

