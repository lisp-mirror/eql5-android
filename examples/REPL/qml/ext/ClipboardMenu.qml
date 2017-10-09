import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Window 2.2
import "." as Ext

Popup {
    x: (Screen.desktopAvailableWidth - width) / 2
    y: 4

    Row {
        id: menuButtonRow
        spacing: 6

        Ext.MenuButton {
            objectName: "select_all"
            text: "\uf07d"
        }
        Ext.MenuButton {
            objectName: "cut"
            text: "\uf0c4"
        }
        Ext.MenuButton {
            objectName: "copy"
            text: "\uf0c5"
        }
        Ext.MenuButton {
            objectName: "paste"
            text: "\uf0ea"
        }
        Ext.MenuButton {
            objectName: "eval_exp"
            text: "\u03bb" // lambda
        }
    }
}

