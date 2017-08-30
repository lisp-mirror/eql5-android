A real keyboard, a real Emacs!
------------------------------

After spending some time trying to hack something useful on android, you simply
can't but agree with the headline!



### Prepare

#### * prepare android:

Currently you need a development version of Swank, so please download the
`zip` file from here directly onto your device:

[slime-simpler-communication-style](https://github.com/luismbo/slime/tree/simpler-communication-style)

Then do in our REPL (the first `zip` file path is just an example):

```
  (quicklisp)

  (ql:quickload :zip)

  (zip:unzip "/storage/emulated/0/Download/slime-simpler-communication-style.zip"
             "quicklisp/local-projects/")
```

Especially the last command will take some time.

Now Quicklisp will load the above Swank version.

#### * prepare PC:

On the PC just use the current Slime (v2.19 at the time of writing).

I needed to manually copy the file `slime-simpler-communication-style/slime.el`
from the above mentioned `zip` file to my Slime directory, e.g.
`quicklisp/dists/quicklisp/software/slime-v2.19/`, in order to have the new
communication style `:loop` work with Slime.



### Connecting to android: 2 variants

Please note: if -- for any reason -- the connection can't be established the
first time you try, it seems best to restart the android REPL, before
attempting another try (I'm talking out of experience).

Also, stopping Swank on android and restarting it, without restarting the REPL
too, seems not to work.

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
this REPL example as a template; you'll basically just need the `assets/`
files and `lisp/ini.lisp`, in order to have Quicklisp and Swank.

Then, after having developed a working desktop version, you can try to change
things (using Slime) directly on the device (using `qml-get`, `qml-set`,
`qml-call` etc.); just make sure to set an `objectName` to every QML item you
want to change interactively.
