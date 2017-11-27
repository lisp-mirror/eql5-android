QT          = widgets printsupport uitools quick quickwidgets qml androidextras
TEMPLATE    = app
TARGET      = sokoban_repl
DESTDIR     = ./android-build/libs/armeabi-v7a
OBJECTS_DIR = ./tmp/

HEADERS     += build/load.h
SOURCES     += build/load.cpp

RESOURCES   = load.qrc

ANDROID_PACKAGE_SOURCE_DIR = android-sources
