(in-package :cl-user)

(defpackage :editor
  (:use :cl :eql :qml)
  (:export
   #:*file*
   #:change-font
   #:close-all-parens
   #:eval*
   #:ensure-focus
   #:reload-qml
   #:save-changes
   #:start))

