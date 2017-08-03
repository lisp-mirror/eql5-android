;;; Original file by Sylvain Ageneau for ECL.
;;;
;;; This is a much simplified version for EQL5.
;;; If you are on 64 bit Linux, you should not need to modify/adapt anything.
;;;
;;; (In a future version, there will be integration with ASDF, as in the
;;; original version.)

(require :cmp)

(defpackage :cross
  (:use :common-lisp)
  (:export
   #:compile-file*
   #:build-static-library*))

(in-package :cross)

(defmacro with-android-env (() &body body)
  `(let* ((ndk (ext:getenv "ANDROID_NDK_ROOT"))
          (ver "android-14")
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

(defun compile-file* (file)
  (with-android-env ()
    (compile-file file :system-p t)))

(defun build-static-library* (name files &optional epilogue-code)
  (with-android-env ()
    (c:build-static-library name
                            :lisp-files files
                            :init-name "ini_app"
                            :epilogue-code epilogue-code)))

