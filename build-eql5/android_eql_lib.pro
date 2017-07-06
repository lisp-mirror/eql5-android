QT            += widgets printsupport uitools
TEMPLATE      = lib
CONFIG        += dll no_keywords release
DEFINES       += EQL_LIBRARY
ECL_ANDROID   = $$(ECL_ANDROID)
INCLUDEPATH   += $$ECL_ANDROID/include
LIBS          += -L$$ECL_ANDROID/lib -lecl -L. -landroid_ini_eql5
TARGET        = eql5
DESTDIR       = ./android-libs/
OBJECTS_DIR   = ./tmp/
MOC_DIR       = ./tmp/

HEADERS += gen/_lobjects.h \
           gen/_main_q_classes.h \
           gen/_main_n_classes.h \
           gen/_main_q_methods.h \
           gen/_main_n_methods.h \
           eql5/eql.h \
           eql5/eql_fun.h \
           eql5/eql_global.h \
           eql5/dyn_object.h \
           qt_eql.h \
           ui_loader.h \
           single_shot.h \
           ecl_fun.h \
           extras.h

SOURCES += gen/_lobjects.cpp \
           qt_eql.cpp \
           dyn_object.cpp \
           ecl_fun.cpp \
           extras.cpp \
           eql.cpp

gcc {
    QMAKE_CXXFLAGS_WARN_ON += -Wno-clobbered
}

clang {
    QMAKE_CXXFLAGS += -std=c++11
}
