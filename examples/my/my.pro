QT          = widgets printsupport uitools quick quickwidgets qml androidextras
TEMPLATE    = app
CONFIG      += no_keywords release
ECL_ANDROID = $$(ECL_ANDROID)
INCLUDEPATH += $$ECL_ANDROID/include ../../include
LIBS        += -L$$ECL_ANDROID/lib -lecl -L./build -lapp -L../../lib -leql5
TARGET      = my
DESTDIR     = ./
OBJECTS_DIR = ./tmp/
MOC_DIR     = ./tmp/

SOURCES     += build/main.cpp

RESOURCES   = my.qrc

