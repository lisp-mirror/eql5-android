
### Info

The size of a full APK (not using the 'ministro' service) is about **15 MB**.
Therefore it would be nice to provide updates which are very small.

In the included examples, the *real* app (not counting the ECL/Qt5/EQL5
dependencies) is very small: it's all contained in `libqtapp.so`, which is
ca. **400 KB** for the **sokoban-repl** example (including *all compiled Lisp
files* plus *all Qt resources*, like QML files and images).


### Build update

Just build the project lib as usual:

```
  $ ecl-android -shell make
  $ qmake-android my.pro    # your *.pro file
  $ make
```

Then run script number **3**:

```
  $ ./3-copy-update.sh
```

This will copy `libqtapp.so` into the `update/` directory and strip it.


### Deploy

Now copy the above `update/libqtapp.so` to your device. In the app REPL on the
device, enter `:u`: this will call `(eql-user::install-update)`.

After selecting the file in the file dialog, it will be copied into the
home directory of your app (*not* overwriting the original one); after
restarting the app, the new version will be used.


### Note

The above works only manually, and only with apps that include a REPL. Of
course this could be automated, downloading the updates from any location,
using e.g. the convenient `:network` module from EQL5.
