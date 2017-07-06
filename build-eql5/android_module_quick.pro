QT           += quick quickwidgets qml printsupport uitools
TEMPLATE     = lib
CONFIG       += dll no_keywords release
ECL_ANDROID  = $$(ECL_ANDROID)
INCLUDEPATH  += $$ECL_ANDROID/include
LIBS         += -L$$ECL_ANDROID/lib -lecl -Landroid-libs -leql5
TARGET       = eql5_quick
DESTDIR      = ./android-libs/
OBJECTS_DIR  = ./tmp/android_quick/
MOC_DIR      = ./tmp/android_quick/

HEADERS += gen/quick/_ini.h \
           gen/quick/_ini2.h \
           gen/quick/_q_classes.h \
           gen/quick/_n_classes.h \
           gen/quick/_q_methods.h \
           gen/quick/_n_methods.h \
           gen/quick/qml_lisp.h

SOURCES += gen/quick/_ini.cpp \
           gen/quick/qml_lisp.cpp
