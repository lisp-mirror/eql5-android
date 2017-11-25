QT          = widgets printsupport uitools quick quickwidgets qml androidextras
TEMPLATE    = app
TARGET      = clock
DESTDIR     = ./android-build/libs/armeabi-v7a
OBJECTS_DIR = ./tmp/

SOURCES     += build/load.cpp

RESOURCES   = load.qrc
