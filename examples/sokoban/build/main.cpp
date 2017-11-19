#undef SLOT

#include <ecl/ecl.h>
#include <eql5/eql.h>
#include <QApplication>
#include <QTextCodec>
#include <QFileInfo>

extern "C" {

void ini_app(cl_object);

void ini() {
    //qApp->setOrganizationName("MyTeam");
    //qApp->setOrganizationDomain("my-team.org");
    qApp->setApplicationName(QFileInfo(qApp->applicationFilePath()).baseName());

    QTextCodec* utf8 = QTextCodec::codecForName("UTF-8");
    QTextCodec::setCodecForLocale(utf8);

    EQL eql;
    eql.exec(ini_app);

    CL_CATCH_ALL_BEGIN(ecl_process_env()) {
        QApplication::exec(); }
    CL_CATCH_ALL_END; }

}
