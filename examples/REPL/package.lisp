(in-package :cl-user)

(defpackage :editor
  (:use :cl :eql :qml)
  (:export
   #:*file*
   #:change-font
   #:eval*
   #:log-output
   #:reload-qml
   #:start))

