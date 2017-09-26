
### Info

Please see both the **sokoban** and **REPL** docu in parent directories.

This is an example of how to add a (very simple) REPL to any android app.

The APK is of course bigger (5.7 MB), because it includes the contribs needed
for Quicklisp etc., plus a working (patched) Swank version.



### Tips

In addition to the docu mentioned above, you can use this function to load a
file using a file dialog:

```
  (dialogs:load-file)
```

If you only want to select a file, call:

```
  (dialogs:get-file-name)         ; for read

  (dialogs:get-file-name :save t) ; for write
```

The selected file is stored in `dialogs:*file-name*`.

--

See also **Reload QML files** in [../REPL/README-1](../REPL/README-1.md).
