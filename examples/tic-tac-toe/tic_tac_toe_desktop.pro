QT          += widgets printsupport uitools quick quickwidgets qml
TEMPLATE    = app
CONFIG      += no_keywords release
INCLUDEPATH += /usr/local/include
LIBS        += -lecl -Lbuild -lapp_desktop -L/usr/local/lib -leql5
TARGET      = tic_tac_toe_desktop
DESTDIR     = ./
OBJECTS_DIR = ./tmp_desktop/
MOC_DIR     = ./tmp_desktop/

SOURCES     += build/main_desktop.cpp

RESOURCES   = tic_tac_toe.qrc
