QT          = widgets printsupport uitools quick quickwidgets qml multimedia multimediawidgets network sensors sql svg androidextras
TEMPLATE    = lib
CONFIG      += plugin no_keywords release
ECL_ANDROID = $$(ECL_ANDROID)
INCLUDEPATH += $$ECL_ANDROID/include ../../include
LIBS        += -L$$ECL_ANDROID/lib -lecl -L./build -lapp -L../../lib -leql5
TARGET      = qtapp
DESTDIR     = ./android-build/libs/armeabi-v7a
OBJECTS_DIR = ./tmp/
MOC_DIR     = ./tmp/

SOURCES     += build/main.cpp

RESOURCES   = qtapp.qrc
