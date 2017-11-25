# ECL, EQL5 libs

mkdir -p android-sources/libs/armeabi-v7a

cp ../../lib/libeql5.so       android-build/libs/armeabi-v7a/
cp ../../lib/libeql5_quick.so android-build/libs/armeabi-v7a/
cp $ECL_ANDROID/lib/libecl.so android-build/libs/armeabi-v7a/
