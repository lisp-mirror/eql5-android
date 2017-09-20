# This is for using QtCreator to build the APK (you need to set the build path
# in QtCreator to this directory).
#
# N.B: This is meant for debugging C/C++ code, and not for deployment!

QT            = widgets printsupport uitools quick quickwidgets qml androidextras
TEMPLATE      = app
CONFIG        += no_keywords debug
ECL_ANDROID   = $$(ECL_ANDROID)
INCLUDEPATH   += $$ECL_ANDROID/include ../../include
LIBS          += -L$$ECL_ANDROID/lib -lecl -L./build -lapp -L../../lib -leql5
TARGET        = debug_repl
DESTDIR       = ./
OBJECTS_DIR   = ./tmp/
MOC_DIR       = ./tmp/

SOURCES       += build/main.cpp

RESOURCES     = repl.qrc

# ECL
ecl.path      = /libs/armeabi-v7a
ecl.files     = $$ECL_ANDROID/lib/libecl.so
INSTALLS      += ecl

# EQL5
eql.path      = /libs/armeabi-v7a
eql.files     = ../../lib/*.so
INSTALLS      += eql

# ECL contrib
contrib.path  = /assets/lib
contrib.files += $$ECL_ANDROID/lib/ecl-16.1.3/*
INSTALLS      += contrib
