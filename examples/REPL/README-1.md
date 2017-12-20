
### Prepare

You need to prepare your android development environment first, as described
in [README-PREPARE](../../README-PREPARE.md).

(see also [known issues](http://wiki.qt.io/Qt_for_Android_known_issues))



### Notes

This example assumes you already tried `../tic-tac-toe` (more detailed
description).

This app has a fixed orientation. You can set this property in
[android-sources/AndroidManifest.xml](android-sources/AndroidManifest.xml):

```
  android:screenOrientation="portrait"
```



### Generate ASDF file dependencies

*Please note that the included ASDF libs are not needed/used by the REPL;
instead, this is only here to show how to integrate eventually needed ASDF
libraries.*

N.B: This needs to be done only once.

This is only a **dummy build** in order to collect all file names in the
right order, and save the file list for cross-compiling.

Before running it, you need to delete 2 directories.

Change to (the equivalent in your home dir):

```
  $ cd ~/.cache/common-lisp/ecl-16.1.3-unknown-linux-x64/home/username
```

Now delete these 2 dirs:

```
  $ rm -fr eql5-android/examples/REPL
  $ rm -fr quicklisp/dists/quicklisp/software
```

That means *deleting all compiled Quicklisp files* (because of eventual
recursive dependencies).

Now run:

```
  $ cd ~/eql5-android/examples/REPL
  $ ecl -shell make-ASDF.lisp
```

This will collect the complete (recursive on dependencies) list of all the
single files which we will need to cross-compile, and save them in `files.txt`.

--

<span style='color: red'><b>This is a hack</b></span> (of course), but it
seems to be the <span style='color: red'><b>least cumbersome</b></span> of
all the other variants of cross-compiling Quicklisp/ASDF systems to android.

Remember that you only need to do this once (needs to be repeated only
on changes of your Quicklisp/ASDF dependencies).

Keep in mind that you can add eventual new source files manually to
`files.txt`, in order to avoid running the above again.

--

Having said that: if you find a better/simpler way to do the above, by all
means, do it your way!



### Make (cross-compile)

Being the whole file list already generated in `files.txt`, cross-compiling is
now a trivial task.

```
  ./1-copy-libs.sh             # copy EQL5, ECL, prebuilt ECL libs (ASDF...)

  ecl-android -shell make.lisp # note 'ecl-android'
  qmake-android repl.pro       # note 'qmake-android'
  make
```

To force recompilation of all files (e.g. after upgrading ECL), pass `-f`
as last argument to the `ecl-android` command.



### Build APK file

The following script will build a full version, including all of Qt5 (both
widgets and QML), because the **ministro** service for the Qt libs seems not
to work on some devices:

```
  ./2-build-apk.sh
```

This will build the APK, which needs to be copied and installed manually
(the reason is that the `--install` option would uninstall all app files,
including eventual previously installed Quicklisp libs etc.).

In order to set the app icons (3 different resolutions) and the app name
you need to run:

```
  $ qtcreator android-sources/AndroidManifest.xml
```

(The above manifest file is a copy of `android-build/AndroidManifest.xml`,
created during the first build of the apk.)



### First launch / prebuilt ECL libs

After launching the app for the first time, all prebuilt ECL libs will be
copied from `assets/` to the home directory (to be easily accessible and
replaceable, just in case).

Now you can `require` them as usual, e.g:

```
  (require :asdf)
  (require :sockets)
```

To get **Quicklisp**, simply eval:

```
  (quicklisp)
```

In the status bar at the bottom you can follow the progress (although it will
only show the last line of the output you would see on a console).



### Copy, Paste, Eval one expression

(works both in the editor and the command line)

Tap and hold on the beginning (or inside) of an expression (e.g. on `defun`) to
select the whole expression, and to show the popup menu for copy and paste. If
you paste the expression to a different indentation level, the indentation will
be automatically adapted.

To eval the selected expression only, just click on the lambda button of the
popup menu.

The icons for this menu are taken from `fontawesome-webfont.ttf` (included).



### Moving the cursor

There is a hidden option for moving the cursor using convenient arrow buttons:
when the virtual keyboard is shown, hide it by tapping on the **back** button
(triangle) of the device.

Now you can move the cursor in both the editor and the command line (where up
and down will move the cursor to beginning and end, respectively). Switch back
to the keyboard using the **keyboard** button.

Tap-and-hold for moving to begin/end of line (left/right) or file (up/down).

(If you need to train your muscle memory for the cursor buttons, just play
Sokoban from the neighbor directory ;-)



### Desktop notes

To run this example on the desktop, do:

```
  $ eql5 run.lisp
```

N.B: Don't use command line option `-qtpl`, as this would interfere with the
debug dialog.



### Reload QML files from android

You can reload the QML files directly from the REPL, editing them on your PC.

Switch to the REPL example directory, and run (after connecting the device
through USB):

```
  $ adb reverse tcp:8080 tcp:8080 # for below; needs android 5
  $ ./web-server.sh               # needs python3
```

Edit e.g. `qml/repl.qml` on the PC and save it; on the android REPL, eval:

```
  (editor:reload-qml)
```

(Reloading is possible because `QQuickView` accepts any URL as source file.)

**Hint**: search the web for `qml-mode` for Emacs, or just use QtCreator,
which is perfect for editing QML files (e.g. `qtcreator my.qml`)



### Tips

When you enter a new file name for saving a file, you may add a path with new
directories: they will be created before saving the file.

If the output window has too much contents (thousands of lines, may happen
accidentally), and is becoming slow, remember that you're using the same Lisp
image where your REPL lives in, so you can clear it directly like this:

```
  (qml-call "output" "clear")
```

For trivial debugging, you can use a simple (blocking) message box from QML
like this (remember that `qmsg` accepts any Lisp value, converting it to a
string if necessary; here: a list):

```
  import EQL5 1.0

  Lisp.call("qmsg", ["var x", x, "var y", y])
```

To evaluate JS code (available in QML), use function `qml:js`:

```
  (qml:js nil "Qt.inputMethod.keyboardRectangle")
```

The first argument is the `this` context of JS; `nil` means the root item.

--

**Quicklisp** tip: you only should eval `(quicklisp)` to install new libraries.

To load them, just use `asdf:load-system`, which is much faster than
`ql:quickload` (on android).



### Slime

Please see [README-SLIME](README-2-SLIME.md)
