(load "~/eql5-android/utils/EQL5-symbols")
(load "android-cross-compile")

(require :cmp)

(setf *break-on-signals* 'error)

(setf c::*compile-in-constants* t)

(defparameter *all-wrappers* (append (loop :for i :from 1 :to 12 :collect (format nil "all-wrappers-~D" i))
                                     (loop :for i :from 1 :to 2 :collect (format nil "all-wrappers-webengine-~D" i))))

(defparameter *lisp-files*   (append (list "x" "package" "ini"
                                           "enums1" "enums2" "enums3" "enums4" "enums5"
                                           "special-extensions")
                                     *all-wrappers*))

(dolist (file *lisp-files*)
  (cross:compile-file* (format nil "lisp/~A.lisp" file)))

(cross:build-static-library* "android_ini_eql5"
                             (mapcar (lambda (file)
                                       (format nil "lisp/~A.o" file))
                                     *lisp-files*))

;; for eql5.pro (doesn't create directories)

(dolist (module (mapcar 'pathname-name (directory "android_module_*.pro")))
  (ensure-directories-exist (format nil "tmp/~A/tmp/"
                                    (x:string-substitute "" "module_" module))))

