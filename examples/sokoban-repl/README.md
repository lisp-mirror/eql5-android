
### Info

Please see both the **sokoban** and **REPL** docu in parent directories.

This is an example of how to add a (very simple) REPL to any android app.

The APK is big here (15 MB), because it includes all of Qt5 (widgets and QML,
since the **ministro** service seems not to work on some devices), plus the
contribs needed for Quicklisp etc., plus a working (patched) Swank version.



### Tips

In addition to the docu mentioned above, you can use this function to load a
file using a file dialog:

```
  (dialogs:load-file)
```

If you only want to select a file, call:

```
  (dialogs:get-file-name)
```

The selected file is stored in `dialogs:*file-name*`.

--

See also **Reload QML files** in [../REPL/README-1](../REPL/README-1.md).
