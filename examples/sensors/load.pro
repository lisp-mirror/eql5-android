QT          = widgets printsupport uitools quick quickwidgets qml sensors androidextras
TEMPLATE    = app
TARGET      = sensors
DESTDIR     = ./android-build/libs/armeabi-v7a
OBJECTS_DIR = ./tmp/

SOURCES     += build/load.cpp

RESOURCES   = load.qrc

ANDROID_PACKAGE_SOURCE_DIR = android-sources
