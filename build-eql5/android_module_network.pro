QT           += network printsupport uitools
TEMPLATE     = lib
CONFIG       += dll no_keywords release
ECL_ANDROID  = $$(ECL_ANDROID)
INCLUDEPATH  += $$ECL_ANDROID/include
LIBS         += -L$$ECL_ANDROID/lib -lecl -Landroid-libs -leql5
TARGET       = eql5_network
DESTDIR      = ./android-libs/
OBJECTS_DIR  = ./tmp/android_network/
MOC_DIR      = ./tmp/android_network/

HEADERS += gen/network/_ini.h \
           gen/network/_ini2.h \
           gen/network/_q_classes.h \
           gen/network/_n_classes.h \
           gen/network/_q_methods.h \
           gen/network/_n_methods.h

SOURCES += gen/network/_ini.cpp
