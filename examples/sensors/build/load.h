#ifndef LOAD_H
#define LOAD_H

#include <QApplication>
#include <QtAndroid> 
#include <QDateTime>

QT_BEGIN_NAMESPACE

class Qt_EQL_Application : public QApplication {
    Q_OBJECT
public:
    Qt_EQL_Application(int argc, char** argv) : QApplication(argc, argv) {}

    // stolen from: https://www.kdab.com/qt-on-android-how-to-restart-your-application/

    Q_INVOKABLE void restartApp() { // callable from Lisp

        auto activity = QtAndroid::androidActivity();

        auto packageManager = activity.callObjectMethod(
            "getPackageManager",
            "()Landroid/content/pm/PackageManager;");
         
        auto activityIntent = packageManager.callObjectMethod(
            "getLaunchIntentForPackage",
            "(Ljava/lang/String;)Landroid/content/Intent;",
            activity.callObjectMethod("getPackageName",
            "()Ljava/lang/String;").object());
         
        auto pendingIntent = QAndroidJniObject::callStaticObjectMethod(
            "android/app/PendingIntent", "getActivity",
            "(Landroid/content/Context;ILandroid/content/Intent;I)Landroid/app/PendingIntent;",
            activity.object(), jint(0), activityIntent.object(),
            QAndroidJniObject::getStaticField<jint>("android/content/Intent",
            "FLAG_ACTIVITY_CLEAR_TOP"));
         
        auto alarmManager = activity.callObjectMethod(
            "getSystemService",
            "(Ljava/lang/String;)Ljava/lang/Object;",
            QAndroidJniObject::getStaticObjectField("android/content/Context",
            "ALARM_SERVICE",
            "Ljava/lang/String;").object());
         
        alarmManager.callMethod<void>(
            "set",
            "(IJLandroid/app/PendingIntent;)V",
            QAndroidJniObject::getStaticField<jint>("android/app/AlarmManager", "RTC"),
            jlong(QDateTime::currentMSecsSinceEpoch() + 1000), pendingIntent.object()); } // wait 1 sec
};

QT_END_NAMESPACE

#endif
