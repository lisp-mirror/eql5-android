
Aknowledgements
---------------

Important: prepare for several GB of downloads (1 GB Qt5, 1 GB Android NDK,
and a few other downloads...).

Please note that I'm assuming a 64 bit Linux or VirtualBox image.

This project requires *exactly* the following development environment:
(in order to match the pre-built EQL5 libraries for Android)

* ECL 16.1.3 [official release](https://common-lisp.net/project/ecl/static/files/release/ecl-16.1.3.tgz) (not a git snapshot)
* Qt 5.7.1 [linux-x64-android](https://download.qt.io/archive/qt/5.7/5.7.1/)
* Android NDK version 10e

(Your desktop EQL5 is completely independent from this project; you don't even
need it to compile Android apps; a **host ECL** will be sufficient, see below.)

N.B: You will *not* need to build the EQL5 shared libraries for Android; the
cross-compiled versions are already included (see `lib/`).
But if you prefer to build them by yourself, please see `build-eql5/`.

Since the included EQL5 Android libraries have been built with the above
versions, you're required to have the exact same versions installed.

Please note: EQL5 for Android is basically the same as EQL5 for the desktop.
Some modules are not supported on Android, namely the web and multimedia ones.

--

The minimum android **API** level is **13** (needed for Qt5), which means the
android device must run at least **version 3.2** (which dates back to 2011).



Step 1: Install all Android development tools
---------------------------------------------

A good description of what you'll need can be found here:

[http://doc.qt.io/qt-5/androidgs.html](http://doc.qt.io/qt-5/androidgs.html)

Please remember to choose this exact **NDK** version: **10e**.

Note that you can skip the 'ant' installation (obsolete, we will use 'gradle'
instead, which will be installed automatically later on).

Also, we don't need Android Studio, so search for **command line tools** on the
SDK download page; for Qt < 5.9, see note below.

The SDK tools are expected to be installed here: `/opt/android/sdk/` (so unzip
them there).

--

N.B: If you use Qt versions prior to 5.9, you need to install an older version
of the SDK tools (the latest one will not work with Qt < 5.9).

Here are the links to the **SDK** tools for Qt prior to 5.9:

[Linux](https://dl.google.com/android/repository/tools_r25.2.5-linux.zip)  
[macOS](https://dl.google.com/android/repository/tools_r25.2.5-macosx.zip)  
[Windows](https://dl.google.com/android/repository/tools_r25.2.5-windows.zip)

As mentioned above, just unzip them in `/opt/android/sdk/`.



Step 2. Build cross-compiled ECL for Android
--------------------------------------------

Please see also the following project (you will not need it, but you may find
a few useful hints there):

[ecl-android](https://gitlab.common-lisp.net/ecl/ecl-android)

To quickly build the cross-compiled ECL, just use the 2 scripts included in
this project (see `scripts/`).

These scripts follow the steps described in the 'ecl-android' project
mentioned above, with a small modification.

* extract ECL sources (release version 16.1.3) in e.g. `~/ecl`, renaming
  `ecl-16.1.3` to `android`
* copy the 2 scripts from `scripts/` to `~/ecl/android/`
* do (adapt the path to match your NDK directory):

```
    $ export ANDROID_NDK_ROOT='/home/username/android/android-ndk-r10e'
```

* run both scripts in order

```
   ./1-make-ecl-host.sh
   ./2-make-ecl-android.sh
```

Now you should have your cross-compiled ECL under `~/ecl/android/ecl-android/`,  
and your host ECL (for cross-compiling) under `~/ecl/android/ecl-android-host/`.



Step 3. Settings in your `~/.bashrc`:
-------------------------------------

* environment variables

```
    JAVA_HOME        : path of your Java jdk
    ANDROID_NDK_ROOT : path of NDK version (needs to be 10e)
    ECL_ANDROID      : path of your cross-compiled ECL
```

* aliases

```
    ecl-android      : exe of host ECL (32 bit) for cross-compiling
    qmake-android    : exe of qmake for android arm (included in Qt5)
```

--

Example:

```
  # <snip ~/.bashrc>

  export JAVA_HOME='/home/username/android/jdk1.8.0_131'
  export ANDROID_NDK_ROOT='/home/username/android/android-ndk-r10e'
  export ECL_ANDROID='/home/username/ecl/android/ecl-android'

  alias ecl-android='/home/username/ecl/android/ecl-android-host/bin/ecl -norc'
  alias qmake-android='/home/username/Qt5.7.1/5.7/android_armv7/bin/qmake'

  # </snip ~/.bashrc>
```

Please don't forget to pass `-norc` to your host ECL (otherwise it could
conflict with your standard ECL compiled files, e.g. ASDF).

--

Now you should be able to build `examples/tic-tac-toe/`.

