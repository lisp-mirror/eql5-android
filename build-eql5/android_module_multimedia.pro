QT           += multimedia multimediawidgets printsupport uitools
TEMPLATE     = lib
CONFIG       += dll no_keywords release
ECL_ANDROID  = $$(ECL_ANDROID)
INCLUDEPATH  += $$ECL_ANDROID/include
LIBS         += -L$$ECL_ANDROID/lib -lecl -Landroid-libs -leql5
TARGET       = eql5_multimedia
DESTDIR      = ./android-libs/
OBJECTS_DIR  = ./tmp/android_multimedia/
MOC_DIR      = ./tmp/android_multimedia/

HEADERS += gen/multimedia/_ini.h \
           gen/multimedia/_ini2.h \
           gen/multimedia/_q_classes.h \
           gen/multimedia/_n_classes.h \
           gen/multimedia/_q_methods.h \
           gen/multimedia/_n_methods.h

SOURCES += gen/multimedia/_ini.cpp
