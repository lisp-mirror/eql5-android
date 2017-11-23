# comment out all modules you don't need:

SUBDIRS = lib \
          multimedia \
          network \
          quick \
          sql \
          svg

TEMPLATE = subdirs

lib.file           = android_eql_lib.pro
multimedia.file    = android_module_multimedia.pro
multimedia.depends = lib
network.file       = android_module_network.pro
network.depends    = lib
quick.file         = android_module_quick.pro
quick.depends      = lib
sql.file           = android_module_sql.pro
sql.depends        = lib
svg.file           = android_module_svg.pro
svg.depends        = lib
