
PREPARE
=======

You need to prepare your Android development environment first, as described
in ../../README-PREPARE.txt

(see also http://wiki.qt.io/Qt_for_Android_known_issues)



NOTE
====

This examples assumes you already tried '../tic-tac-toe'.



MAKE (cross-compile for Android)
====

  ./1-copy-libs.sh             # copy EQL5 and ECL libs

  ecl-android -shell make.lisp # note 'ecl-android'
  qmake-android clock.pro      # note 'qmake-android'
  make

  ./2-install-lib.sh           # make install...

If you need to recompile the Lisp code, remember to do 'touch tmp/main.o',
in order to force 'make' to link your newly compiled Lisp library.

Remember to run '2-install-lib.sh' after recompiling.



BUILD APK FILE
==============

  ./3-build-apk.sh



TEST
====

Now you can connect your Android device via the USB port, copy over the *.apk
file, and install/run it.

