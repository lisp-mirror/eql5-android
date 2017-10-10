import QtQuick 2.0
import QtQuick.Controls 2.0

Button {
    width: 60
    height: 55
    font.family: fontAwesome.name
    font.pixelSize: 36
    focusPolicy: Qt.NoFocus

    FontLoader {
        id: fontAwesome
        source: "../fonts/fontawesome-webfont.ttf"
    }
}
