# be careful with option --install, as this would uninstall
# the app first, deleting all files (e.g. from Quicklisp)

~/Qt5.7.1/5.7/android_armv7/bin/androiddeployqt \
  --input android-librepl.so-deployment-settings.json \
  --output android-build \
  --gradle
