
### Info

This is just a basic skeleton for interactively developing android apps.

If you want to use the extreme minimum (that is, not installing any of NDK,
SDK, ECL, Qt5, EQL5), you can just use the **APK** of this example, plus the
standalone **adb** command line tool for android, and Emacs/Slime on your PC,
plus a python 3 installation (or any trivial web server, see script
`web-server.sh`).

* connect your device via USB

* run `$ adb forward tcp:4005 tcp:4005`

* run `$ adb reverse tcp:8080 tcp:8080`

* in this directory run `$ ./web-server.sh` (requires python 3)

Now start the **my** app on android and tap on the REPL switch.

* on the android REPL, eval `:s` (to start Swank)

* on the PC, connect from Slime `M-x slime-connect RET RET`

Now use `lisp/my.lisp` and `qml/my.qml` for development.

* to reload the QML file (after saving the changes) eval `:r` on the PC



### Build APK

```
  $ ecl-android -shell make.lisp
  $ qmake-android my.pro
  $ make

  $ ./1-copy-libs.sh
  $ ./2-install-lib.sh
  $ ./3-build-apk.sh
```

(A more detailed description can be found in example "../tic-tac-toe".)
