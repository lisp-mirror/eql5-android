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

    Q_INVOKABLE bool checkPermission(const QString& name) {
        // e.g. "android.permission.WRITE_EXTERNAL_STORAGE"
        // this handles the new Android permission model, starting with API version 23 (Android 6),
        // that is: we need to ask for the permission at runtime (every time we need a permission)
#if QT_VERSION > 0x050A00 // 5.10
        QtAndroid::PermissionResult res = QtAndroid::checkPermission(name);
        if(res == QtAndroid::PermissionResult::Denied) {
            QtAndroid::requestPermissionsSync(QStringList() << name);
            res = QtAndroid::checkPermission(name);
            if(res == QtAndroid::PermissionResult::Denied) {
                return false; }}
#endif
        return true; }

    Q_INVOKABLE void restartApp() {
        // stolen from: https://www.kdab.com/qt-on-android-how-to-restart-your-application/
        // this will restart the app after a few seconds (not immediately)
        // (uses JNI, Java Native Interface)

        auto activity = QtAndroid::androidActivity();

        auto packageManager = activity.callObjectMethod(
            "getPackageManager", "()Landroid/content/pm/PackageManager;");
         
        auto activityIntent = packageManager.callObjectMethod(
            "getLaunchIntentForPackage", "(Ljava/lang/String;)Landroid/content/Intent;",
            /*L*/ activity.callObjectMethod("getPackageName", "()Ljava/lang/String;").object());
         
        auto pendingIntent = QAndroidJniObject::callStaticObjectMethod(
            "android/app/PendingIntent",
            "getActivity", "(Landroid/content/Context;ILandroid/content/Intent;I)Landroid/app/PendingIntent;",
            /*L*/  activity.object(),
            /*IL*/ jint(0), activityIntent.object(),
            /*I*/  QAndroidJniObject::getStaticField<jint>("android/content/Intent", "FLAG_ACTIVITY_CLEAR_TOP"));
         
        auto alarmManager = activity.callObjectMethod(
            "getSystemService", "(Ljava/lang/String;)Ljava/lang/Object;",
            /*L*/ QAndroidJniObject::getStaticObjectField("android/content/Context", "ALARM_SERVICE", "Ljava/lang/String;").object());
         
        alarmManager.callMethod<void>(
            "set", "(IJLandroid/app/PendingIntent;)V",
            /*IJL*/ QAndroidJniObject::getStaticField<jint>("android/app/AlarmManager", "RTC"),
                    jlong(QDateTime::currentMSecsSinceEpoch() + 1000), // wait 1 sec (for slow devices)
                    pendingIntent.object()); }
};

QT_END_NAMESPACE

#endif
