import QtQuick 2.0
import QtQuick.Controls 2.0

Button {
    width: 56
    height: 56
    font.family: fontAwesome.name
    font.pixelSize: 36
    focusPolicy: Qt.NoFocus

    FontLoader {
        id: fontAwesome
        source: "../fonts/fontawesome-webfont.ttf"
    }
}
