#include <QApplication>
#include <QLibrary>
#include <QLabel>

static bool load(const QString& name) {
    QLibrary lib(name);
    return lib.load(); }

int main(int argc, char** argv) {
    QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
    QApplication qapp(argc, argv);

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
    
    QLibrary qtapp(path.arg("qtapp"));
    typedef void (*Ini)();
    Ini ini = (Ini)qtapp.resolve("ini");
    ini();
    
    return 0; }
