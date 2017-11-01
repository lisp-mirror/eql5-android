import QtQuick 2.0
import QtQuick.Controls 2.0

Button {
    width: main.isPhone ? 30 : 42
    height: width
    font.family: fontAwesome.name
    font.pixelSize: main.isPhone ? 20 : 28
    focusPolicy: Qt.NoFocus
}
