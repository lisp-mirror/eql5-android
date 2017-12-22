import QtQuick 2.0
import QtQuick.Controls 2.0

Button {
    width: main.isPhone ? 30 : 60
    height: main.isPhone ? 40 : 60
    font.pixelSize: main.isPhone ? 15 : 20
    font.bold: true
    focusPolicy: Qt.NoFocus
    flat: true
}
