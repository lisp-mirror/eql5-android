(in-package :cl-user)

(defpackage :editor
  (:use :cl :eql :qml)
  (:export
   #:*file*
   #:log-output
   #:reload-qml
   #:start))

