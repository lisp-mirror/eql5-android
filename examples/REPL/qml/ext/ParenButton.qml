import QtQuick 2.0
import QtQuick.Controls 2.0

Button {
    width: main.isPhone ? 35 : 55
    height: width
    font.pixelSize: main.isPhone ? 24 : 30
    font.bold: true
    focusPolicy: Qt.NoFocus
    flat: true
}
