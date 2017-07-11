
PREPARE
=======

You need to prepare your Android development environment first, as described
in ../../README-PREPARE.txt

(see also http://wiki.qt.io/Qt_for_Android_known_issues)

In order to have all external resources, like qml files and images, included
in the executable, you need to add them to the resource file (already done
here, see 'tic_tac_toe.qrc').



TEST-BUILD (OPTIONAL - SKIP THIS) - build and run a desktop version
=================================

You might want to verify if the app would run on the desktop.

(Or you just wanted to see the slight differences between both builds.)

  eql5 make-desktop.lisp
  qmake tic_tac_toe_desktop.pro
  make

Now you should be able to run:

  ./tic_tac_toe_desktop

To verify if all resources have been included, you need to move it to another
directory and run it from there.



MAKE (cross-compile for Android)
====

  ./1-copy-libs.sh              # copy EQL5 and ECL libs

  ecl-android -shell make.lisp  # note 'ecl-android'
  qmake-android tic_tac_toe.pro # note 'qmake-android'
  make

  ./2-install-lib.sh            # make install...

We use a trick for cross-compiling: since we don't want an EQL5 version only
for cross-compiling, linked to a 32 bit ECL, we 'pretend' to be the 'eql5'
executable, by defining all packages, symbols and macros(!) of EQL5, and
defining dummy functions for all EQL5 functions, so the compiler will not
complain.

This is done by loading the file '../../utils/EQL5-symbols.lisp' into ECL.

There is only one drawback here: we can't use reader macros containing EQL5
code; see e.g. file 'lisp/qml-lisp.lisp', which has them removed, compared to
the desktop version (see EQL5 QML examples).

If you need to recompile the Lisp code, remember to do 'touch tmp/main.o',
in order to force 'make' to link your newly compiled Lisp library.

Remember to run '2-install-lib.sh' after recompiling.



BUILD APK FILE
==============

The Android SDK tools are expected to be installed in '/opt/android/sdk', so
it's a good idea to move them there (Android Studio will install them by
default in '~/Android/Sdk' during the first start of Android Studio).

There is a nice Qt utility, which will automate the whole (and complex) build
process. It's located under your Qt installation directory, e.g.

  ~/Qt5.7.1/5.7/android_armv7/bin/androiddeployqt

Run the tool using the third script:

  ./3-build-apk.sh

This will run the following command:

  androiddeployqt --input android-libtic_tac_toe.so-deployment-settings.json \
                  --output android-build \
                  --deployment ministro \
                  --gradle \
                  --verbose

The mentioned 'json' file is generated automatically during 'make', see above.

'gradle' is the new 'ant'. It will be downloaded the first time you start a
build.

The '--deployment ministro' option means that you don't need to provide the Qt
libraries; they will instead be downloaded automatically the first time the app
is started on the Android device.
(This is of course optional, you may also include the Qt libs, which will push
up the size of the APK file to about 13.5 MB.)

A list of the currently supported Qt versions of 'ministro' can be found here:

  https://download.qt.io/ministro/android/qt5/

At the time of writing, only versions up to Qt 5.7.1 are supported.

Using 'ministro', the APK file is about 3.5 MB (including the EQL5 and ECL
libs), which seems acceptable.

N.B: The first time you run this, you will probably hit a bug of the tool; so,
just open the file 'android-build/build.gradle', and edit the offending line;
change the property 'androidCompileSdkVersion' to:

  androidCompileSdkVersion=26

(that is, the version of your SDK tools, see e.g. Android Studio settings.)

Now the build should succeed (verbosely, as requested). The APK package is in
'android-build/build/outputs/apk/'.



TEST
====

Now you can connect your Android device via the USB port, copy over the *.apk
file, and install/run it.

(Your Android device needs special developer settings in order to do this,
which are different for every Android version. The web is your friend...)



DEBUG
=====

'/opt/android/sdk/platform-tools/adb' can be useful for debugging, e.g:

  adb logcat

will print logging messages of the device.

For simple debugging on Android you may also just use a message box:

  (qmsg variable-or-message)

Debugging using Qt Creator is probably the most comfortable way, if you get
it to work.

