#include "load.h"
#include <QDir>
#include <QFile>
#include <QLabel>
#include <QLibrary>

QT_BEGIN_NAMESPACE

static bool load(const QString& name) {
    QLibrary lib(name);
    return lib.load(); }

int main(int argc, char** argv) {
    QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
    Qt_EQL_Application qapp(argc, argv);

    // splash pixmap (see "../../../img/logo.png")
    QLabel* splash = new QLabel;
    splash->setPixmap(QPixmap(":/img/logo.png"));
    splash->setAlignment(Qt::AlignCenter);
    splash->show();
    qApp->processEvents();
    splash->deleteLater();

    /*** workaround for android 4.2 etc. ***/

    // pre-load all libs manually, providing the absolute pathname
    QString path(QCoreApplication::applicationDirPath() + "/lib%1.so");
    load(path.arg("ecl"));
    load(path.arg("eql5"));
    load(path.arg("eql5_quick"));
    //load(path.arg("eql5_multimedia"));
    //load(path.arg("eql5_network"));
    //load(path.arg("eql5_sql"));
    //load(path.arg("eql5_svg"));

    // we are prepared for eventual updates; the app itself is fully contained
    // in 'libqtapp.so' (all of compiled Lisp files plus Qt resource files)
    QString file(QDir::homePath() + "/update/libqtapp.so"); // ev. updated version
    if(!QFile::exists(file)) {
        file = path.arg("qtapp"); }                         // default version
    QLibrary qtapp(file);
    typedef void (*Ini)();
    Ini ini = (Ini)qtapp.resolve("ini");
    ini();

    return 0; }

QT_END_NAMESPACE
