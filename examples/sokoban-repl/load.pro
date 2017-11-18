QT          = widgets printsupport uitools quick quickwidgets qml network sensors sql svg androidextras
TEMPLATE    = app
TARGET      = sokoban_repl
DESTDIR     = ./
OBJECTS_DIR = ./tmp/

SOURCES     += build/load.cpp

RESOURCES   = load.qrc

ANDROID_PACKAGE_SOURCE_DIR = android-sources
