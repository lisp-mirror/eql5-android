### Prepare

#### Android

Since a working (and patched) Swank version is already included, just eval
`(start-swank)`, or simply `:s`, to start the server.

#### PC

On the PC you should preferably use Slime v2.19 (newer versions may work, but
will show a warning on startup).



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

* on the android REPL, run:

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

* on the android REPL, run:

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

You can either use the EQL5 desktop version to build your app on the desktop
first, or you use the approach shown in example `../my`, which is fully
interactive for both Lisp and QML.

Example `../sokoban-repl` integrates a simple REPL, hidden by default (the
same REPL as in example `../my`).

See also file `lisp/qml-lisp` for interacting with QML, especially
`(qml:reload)`. Make sure to set an `objectName` to every QML item you
want to change interactively.

--

Please see also [README](README-1.md), **"Reload QML files from android"**
