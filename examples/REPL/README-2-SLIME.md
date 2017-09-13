### IMPORTANT NOTE

You **can't** _(currently)_ use the **Slime REPL**, as it would randomly
**freeze** both the android app and Emacs.

That may sound worse than it is: if you use a scratch file as a pseudo REPL,
everything works without problems (auto-completion etc.); just use `C-M-x` to
eval expressions (the cursor must be inside of them), or `C-c C-e` for simple
interactive eval (with command history, see arrow keys).

*So, don't (currently) touch the REPL. You have been warned.*



### Prepare

#### * prepare android:

Currently (that is, as long as the new `:spawn` communication style doesn't
work on android), you need a tiny patch in `swank.lisp`:

```
 (defun repl-input-stream-read (connection stdin)
   (loop
    (let* ((socket (connection.socket-io connection))
+          (inputs (list socket #-android stdin))
-          (inputs (list socket stdin))
           (ready (wait-for-input inputs)))
      (cond ((eq ready :interrupt)
             (check-slime-interrupts))
```

In order to install Swank, you need to run `(start-swank)` first, without
connecting from Slime (before applying the patch).

The easiest way to apply it seems to patch your `swank.lisp` on the PC, then
copy it over to android; from our android REPL app, eval (adapting the paths):

```
  (let ((from "/storage/emulated/0/Documents/swank.lisp")
        (to "quicklisp/dists/quicklisp/software/slime-v2.20/swank.lisp"))
    (ignore-errors (delete-file to))
    (|copy.QFile| from to))
```

The above function needs to return `T` as first value, otherwise it didn't copy
the file.

#### * prepare PC:

On the PC just use the current Slime (v2.20 at the time of writing).



### Connecting to android: 2 variants

Please note: if -- for any reason -- the connection can't be established the
first time you try, it seems best to restart the android REPL, before
attempting another try (I'm talking out of experience).

Also, stopping Swank on android and restarting it, without restarting the REPL
app too, seems not to work.

#### 1) Local: port forwarding

The `adb` command is normally located in `/opt/android/sdk/platform-tools/`.

You only need to physically connect your device via USB (no wlan needed).

* enable port forwarding

```
  $ adb forward tcp:4005 tcp:4005
```

* on the android REPL, run (will be slow, you'll need some patience)

```
  (start-swank)
```

* connect from Emacs:

```
  M-X slime-connect RET RET (just use default values)
```



#### 2) Remote: use ssh tunnel

You'll need an ssh app, e.g. SSHDroid; run it and activate the ssh daemon.
It will tell you the IP address of your device.

* on the android REPL, run (will be slow, you'll need some patience)

```
  (start-swank :loopback "127.0.0.1")
```

* on the PC, create the ssh tunnel, using the IP (after `root@`) as shown in
  the ssh app

```
  $ ssh -L4005:127.0.0.1:4005 root@192.168.1.2 -p 2222
```

(the default password is `admin`)

* connect from Emacs:

```
  M-X slime-connect RET RET (just use default values)
```



### Testing

To test if everything works, you can try (for the fun of it):

```
  (use-package :qml)

  (qml-set "edit" "text" "(sqrt -2)")

  (qml-call "eval" "clicked")
```

To get the text (or better: html) of the output window, do:

```
  (qml-get "output" "text")
```



### Developing a GUI interactively, using QML

Well, you might be tempted to do this directly on the device: but this is not
a good idea (if you understood the nature of Qt5); simply develop your app on
the desktop first!

Please see file `lisp/qml-lisp` for interacting with QML, especially
`(qml:reload)`.

In order to be able to interact with your app running on the device, just use
this REPL example as a template; you'll basically just need
`android-build/assets/lib/*`, `lisp/ini.lisp`, `build/main.cpp` to be able to
install Quicklisp and Swank.

For an example of the above (including a very basic REPL, hidden by default),
please see example `../sokoban-repl`.

Then, after having developed a working desktop version, you can try to change
things (using Slime) directly on the device (using `qml-get`, `qml-set`,
`qml-call` etc.); just make sure to set an `objectName` to every QML item you
want to change interactively.

--

Please see also [README](README-1.md), **"Reload QML files from android"**
