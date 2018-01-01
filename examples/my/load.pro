QT          = widgets printsupport uitools quick quickwidgets qml multimedia multimediawidgets network positioning sensors sql svg webview androidextras
TEMPLATE    = app
TARGET      = my
DESTDIR     = ./android-build/libs/armeabi-v7a
OBJECTS_DIR = ./tmp/

HEADERS     += build/load.h
SOURCES     += build/load.cpp

RESOURCES   = load.qrc

ANDROID_PACKAGE_SOURCE_DIR = android-sources
