
INFO
====

If you don't want to use the included EQL5 libs from '../lib/' (or you want to
use a Qt version different from 5.7.1), just follow the below steps.



IMPORTANT NOTES
===============

You will need EQL5 version >= 17.7.1 (check eql5 -v).

For developing Android apps, simply stay with the exact same ECL/Qt5/EQL5
versions you used to build the cross-compiled EQL5 libs.

(Should be obvious, but I wanted to repeat it.)



CROSS-COMPILE EQL5
==================

This will compile all supported eql5 libs (I'm assuming you already went
through '../README-PREPARE.txt').

- copy all files from this directory to your eql5 desktop installation, and
  switch to it:

  $ cp * ~/eql5/src/
  $ cd ~/eql5/src

Edit file 'android-make.lisp' and adapt the path to your 'EQL5-symbols.lisp'
file (first line); then do:

  $ ecl-android -shell android-make.lisp
  $ qmake-android android_eql5.pro
  $ make

This will start to compile, but then it will give you a (harmless) error
message like:

----------------------------------------------------------------------------
qmetatype.h:2203:100: error: conflicting declaration 'void* __dso_handle'
           gc.h:1625:24: note: previous declaration as 'int __dso_handle []'
----------------------------------------------------------------------------

Now you'll need to patch the offending 'gc.h' file from Android ECL; here's
the diff:

---------------------------------------------------------------------------
<   extern int _etext[], __dso_handle[];
---
>   // extern int _etext[], __dso_handle[]; // keep original line
>   extern int _etext[];                    // same as original
>   extern void* __dso_handle;              // match it with previous decl.
---------------------------------------------------------------------------

Retrying 'make' should continue until module 'quick', giving this error:

  "error: undefined reference to 'QSGTexture::bind()'"

Now you need to edit file './gen/quick/_q_classes.h' and search:

  class LSGDynamicTexture

Comment out the following functions:

  // void bind() {...}
  // bool hasAlphaChannel() const {...}
  // bool hasMipmaps() const {...}
  // int textureId() const {...}
  // QSize textureSize() const {...}

Now 'make' should finally be successful.

The last step is stripping (to be ready for deployment):

  $ cd android-libs
  $ $ANDROID_NDK_ROOT/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin/arm-linux-androideabi-strip lib*

Finally copy the libs over to 'eql5-android':

  $ cp lib* ~/eql5-android/lib/



NOTE
====

You may have noticed that there's no executable included, like 'eql5' on the
desktop; even if you try to compile it, it would still build a shared library
(because this is the way Android works -- your Qt 'main()' function will be
called from Java when launching an app).

