#undef SLOT

#include <ecl/ecl.h>
#include <eql5/eql.h>
#include <QApplication>
#include <QTextCodec>
#include <QFileInfo>
#include <QLabel>

extern "C" void ini_app(cl_object);

int catch_all_qexec() {
    int ret = 0;
    CL_CATCH_ALL_BEGIN(ecl_process_env()) {
        ret = QApplication::exec(); }
    CL_CATCH_ALL_END;
    return ret; }

int main(int argc, char** argv) {

    EQL::ini(argv); // best initialized here

    QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
    QApplication qapp(argc, argv);

    //qapp.setOrganizationName("MyTeam");
    //qapp.setOrganizationDomain("my-team.org");
    qapp.setApplicationName(QFileInfo(qapp.applicationFilePath()).baseName());

    QTextCodec* utf8 = QTextCodec::codecForName("UTF-8");
    QTextCodec::setCodecForLocale(utf8);

    // splash pixmap (see "../../../img/logo.png")
    QLabel* splash = new QLabel;
    splash->setPixmap(QPixmap(":/img/logo.png"));
    splash->setAlignment(Qt::AlignCenter);
    splash->show();
    qApp->processEvents();
    splash->deleteLater();

    EQL eql;
    eql.exec(ini_app);

    return catch_all_qexec(); }
