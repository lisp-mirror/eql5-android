#undef SLOT

#include <ecl/ecl.h>
#include <eql5/eql.h>
#include <QApplication>
#include <QTextCodec>
#include <QFileInfo>

extern "C" {

void ini_app(cl_object);

// N.B. unique ini function name (see 'load.cpp')

void ini_CL_REPLay() {
    //qApp->setOrganizationName("MyTeam");
    //qApp->setOrganizationDomain("my-team.org");
    qApp->setApplicationName(QFileInfo(qApp->applicationFilePath()).baseName());

    QTextCodec* utf8 = QTextCodec::codecForName("UTF-8");
    QTextCodec::setCodecForLocale(utf8);

    EQL eql;
    // we need a fallback restart for connections from Slime
    eql.exec(ini_app, "(loop (with-simple-restart (restart-qt-events \"Restart Qt event processing.\") (qexec)))"); }

}
