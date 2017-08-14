;;; Original file by Sylvain Ageneau for ECL.
;;;
;;; This is a much simplified version for EQL5.
;;; If you are on 64 bit Linux, you should not need to modify/adapt anything.
;;;
;;; (See example 'REPL' for integrating with  ASDF/Quicklisp)

(require :cmp)

(defpackage :cross
  (:use :common-lisp)
  (:export
   #:compile-file*
   #:build-static-library*))

(in-package :cross)

(defmacro with-android-env (() &body body)
  `(let* ((ndk (ext:getenv "ANDROID_NDK_ROOT"))
          (ver "android-13")
          (toolchain (x:cc ndk "/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64"))
          (sysroot (x:cc ndk "/platforms/" ver))
          (ecl-android (ext:getenv "ECL_ANDROID"))
          (compiler::*ecl-include-directory* (x:cc ecl-android "/include/"))
          (compiler::*ecl-library-directory* (x:cc ecl-android "/lib/"))
          (compiler::*cc* (x:cc toolchain "/bin/arm-linux-androideabi-gcc"))
          (compiler::*ld* (x:cc toolchain "/bin/arm-linux-androideabi-gcc"))
          (compiler::*ar* (x:cc toolchain "/bin/arm-linux-androideabi-ar"))
          (compiler::*ranlib* (x:cc toolchain "/bin/arm-linux-androideabi-ranlib"))
          (compiler::*cc-flags* (x:join (list "-g"
                                              (x:cc "--sysroot=" sysroot)
                                              "-DANDROID -DPLATFORM_ANDROID"
                                              "-O2 -fPIC -fno-common -D_THREAD_SAFE"
                                              (x:cc "-I" ndk "/platforms/" ver "/arch-arm/usr/include")
                                              (x:cc "-I" ecl-android "/build/gmp"))))
          (compiler::*ld-flags* (x:join (list "-g"
                                              (x:cc "--sysroot=" sysroot)))))
     ,@body))

(defun compile-file* (file &optional (system-p t))
  (with-android-env ()
    (compile-file file :system-p system-p)))

(defun build-static-library* (name &rest arguments)
  (with-android-env ()
    (apply 'c:build-static-library name arguments)))

