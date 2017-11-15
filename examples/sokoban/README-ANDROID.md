
### Prepare

You need to prepare your android development environment first, as described
in [README-PREPARE](../../README-PREPARE.md).

(see also [known issues](http://wiki.qt.io/Qt_for_Android_known_issues))



### Note

This examples assumes you already tried `../tic-tac-toe`.



### Make (cross-compile)

```
  ./1-copy-libs.sh             # copy EQL5 and ECL libs

  ecl-android -shell make.lisp # note 'ecl-android'
  qmake-android sokoban.pro    # note 'qmake-android'
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

