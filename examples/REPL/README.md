
### Prepare

You need to prepare your Android development environment first, as described
in `../../README-PREPARE.md`

(see also [known issues](http://wiki.qt.io/Qt_for_Android_known_issues))



### Note

This examples assumes you already tried `../tic-tac-toe` (more detailed
description).



### Generate ASDF file dependencies

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
  ./1-copy-libs.sh             # copy EQL5 and ECL libs

  ecl-android -shell make.lisp # note 'ecl-android'
  qmake-android repl.pro       # note 'qmake-android'
  make

  ./2-install-lib.sh           # make install...
```

If you need to recompile the Lisp code, remember to do `touch tmp/main.o`,
in order to force `make` to link your newly compiled Lisp library.

Remember to run `2-install-lib.sh` after recompiling.



### Build APK file

```
  ./3-build-apk.sh
```


### Desktop notes

To run this example on the desktop, do:

```
  $ eql5 run.lisp
```

N.B: Don't use command line option `-qtpl`, as this would interfere with the
debug dialog.

Note that the selected font "Droid Sans Mono" is obviously not available on
the desktop.

Note also that during development, you should change this line in
`lisp/eval.lisp`:

```
  (defvar *silent* nil) ; T for deployment, NIL for development
```



### Tips

On first startup, if you don't see a horizontal line (dividing input and
output), you may need to press the `Clear` button first; then tap in the input
field above, to show the virtual keyboard.

If the output window has too much contents (thousands of lines, may happen
accidentally), and is becoming slow, remember that you're using the same Lisp
image where your REPL lives in, so you can clear it directly like this:

```
  (qml:qml-call "output" "clear")
```
