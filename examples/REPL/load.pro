QT          = widgets printsupport uitools quick quickwidgets qml network androidextras
TEMPLATE    = app
TARGET      = repl
DESTDIR     = ./android-build/libs/armeabi-v7a
OBJECTS_DIR = ./tmp/
MOC_DIR     = ./tmp/

HEADERS     += build/load.h
SOURCES     += build/load.cpp

RESOURCES   = load.qrc

ANDROID_PACKAGE_SOURCE_DIR = android-sources
