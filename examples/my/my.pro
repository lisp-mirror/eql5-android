TEMPLATE    = subdirs
SUBDIRS     = load app

app.file    = qtapp.pro
load.file   = load.pro
app.depends = load
