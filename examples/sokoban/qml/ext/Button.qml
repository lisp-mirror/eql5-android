import QtQuick 2.7
import QtQuick.Controls 2.0

Button {
    width: main.isPhone ? 32 : 50
    height: width
    font.pixelSize: width - 6
    opacity: 0.8
}
