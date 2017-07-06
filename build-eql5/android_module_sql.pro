QT           += sql printsupport uitools
TEMPLATE     = lib
CONFIG       += dll no_keywords release
ECL_ANDROID  = $$(ECL_ANDROID)
INCLUDEPATH  += $$ECL_ANDROID/include
LIBS         += -L$$ECL_ANDROID/lib -lecl -Landroid-libs -leql5
TARGET       = eql5_sql
DESTDIR      = ./android-libs/
OBJECTS_DIR  = ./tmp/android_sql/
MOC_DIR      = ./tmp/android_sql/

target.path  = ../android-libs
INSTALLS     = target

HEADERS += gen/sql/_ini.h \
           gen/sql/_ini2.h \
           gen/sql/_q_classes.h \
           gen/sql/_n_classes.h \
           gen/sql/_q_methods.h \
           gen/sql/_n_methods.h

SOURCES += gen/sql/_ini.cpp
