import QtQuick 2.7
import QtQuick.Controls 2.0

Button {
    width: main.isPhone ? 42 : 48
    height: width
    font.family: fontAwesome.name
    font.pixelSize: 24
    flat: true
}
