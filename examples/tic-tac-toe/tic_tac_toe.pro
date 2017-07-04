QT             += widgets printsupport uitools quick quickwidgets qml androidextras
TEMPLATE       = app
CONFIG         += no_keywords release
ECL_ANDROID    = $$(ECL_ANDROID)
INCLUDEPATH    += $$ECL_ANDROID/include /usr/local/include
LIBS           += -L$$ECL_ANDROID/lib -lecl -L./build -lapp -L../../lib -leql5
TARGET         = tic_tac_toe
DESTDIR        = ./
OBJECTS_DIR    = ./tmp/
MOC_DIR        = ./tmp/

OTHER_FILES    += qml/tic-tac-toe.qml \
                  qml/ext/Button.qml \
                  qml/ext/TicTac.qml

SOURCES        += build/main.cpp

RESOURCES      = tic_tac_toe.qrc

