
### Info

This is just a basic skeleton for interactively developing android apps: so,
it's perfect for playing around, without installing some GB of development
tools first...

If you want to **use the extreme minimum** (that is, **not** installing any of
NDK, SDK, ECL, Qt5, EQL5, EQL5-Android), you can just use the
[APK](http://lights-of-holiness.eu/tmp/my.apk) of this example, plus the
standalone
[adb](https://developer.android.com/studio/releases/platform-tools.html)
command line tool for android, and Emacs/Slime on your PC, plus a python 3
installation (or any trivial web server, see script `web-server.sh`).

* connect your device via USB

* run `$ adb forward tcp:4005 tcp:4005`

* run `$ adb reverse tcp:8080 tcp:8080`

* in this directory run `$ ./web-server.sh` (requires python 3)

Now start the **my** app on android and tap on the REPL switch.

* on the android REPL, eval `:s` (to start Swank)

* on the PC, connect from Slime `M-x slime-connect RET RET`

Now use `lisp/my.lisp` and `qml/my.qml` for development.

* to reload the QML file (after saving the changes) eval `:r` on the PC

(Sometimes the Slime connection doesn't work the first time you try to connect;
in this case you need to restart either Emacs or the android app; if in doubt,
just restart both of them.)


### Build APK

(this obviously needs a full installation of all tools)

```
  $ ecl-android -shell make.lisp
  $ qmake-android my.pro
  $ make

  $ ./1-copy-libs.sh
  $ ./2-install-lib.sh
  $ ./3-build-apk.sh
```

(a more detailed description can be found in example `../tic-tac-toe`)
