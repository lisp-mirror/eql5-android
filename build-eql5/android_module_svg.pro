QT           += svg printsupport uitools
TEMPLATE     = lib
CONFIG       += dll no_keywords release
ECL_ANDROID  = $$(ECL_ANDROID)
INCLUDEPATH  += $$ECL_ANDROID/include
LIBS         += -L$$ECL_ANDROID/lib -lecl -Landroid-libs -leql5
TARGET       = eql5_svg
DESTDIR      = ./android-libs/
OBJECTS_DIR  = ./tmp/android_svg/
MOC_DIR      = ./tmp/android_svg/

target.path  = ../android-libs
INSTALLS     = target

HEADERS += gen/svg/_ini.h \
           gen/svg/_ini2.h \
           gen/svg/_q_classes.h \
           gen/svg/_n_classes.h \
           gen/svg/_q_methods.h \
           gen/svg/_n_methods.h

SOURCES += gen/svg/_ini.cpp
