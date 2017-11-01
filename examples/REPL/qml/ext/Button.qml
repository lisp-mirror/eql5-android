import QtQuick 2.0
import QtQuick.Controls 2.0

Button {
    width: main.isPhone ? 40 : 60
    height: main.isPhone ? width : 55
    font.family: fontAwesome.name
    font.pixelSize: main.isPhone ? 25 : 36
    focusPolicy: Qt.NoFocus

    FontLoader {
        id: fontAwesome
        source: "../fonts/fontawesome-webfont.ttf"
    }
}
