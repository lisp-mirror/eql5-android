
### Introduction

This uses a `PaintedItem`, available with `import EQL5 1.0`.

Note that if you use a `PaintedItem` (for custom painting), you need to
remember:

 * use a `QQuickWidget` instead of a `QQuickView` (see [qml-lisp](lisp/qml-lisp.lisp))
 * define an initial `width` and `height` in your QML main item
 * performance and animations will not be as smooth as with `QQuickView`



### Prepare

You need to prepare your Android development environment first, as described
in `../../README-PREPARE.md`

(see also [known issues](http://wiki.qt.io/Qt_for_Android_known_issues))



### Notes

This examples assumes you already tried `../tic-tac-toe`.



### Make (cross-compile)

```
  ./1-copy-libs.sh             # copy EQL5 and ECL libs

  ecl-android -shell make.lisp # note 'ecl-android'
  qmake-android clock.pro      # note 'qmake-android'
  make
```



### Build APK file

```
  ./2-build-apk.sh
```

