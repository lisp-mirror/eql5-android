mkdir android-build
mkdir android-build/libs
mkdir android-build/libs/armeabi-v7a

cp ../../lib/libeql5.so         android-build/libs/armeabi-v7a/
cp ../../lib/libeql5_quick.so   android-build/libs/armeabi-v7a/
cp ../../lib/libeql5_network.so android-build/libs/armeabi-v7a/
cp ../../lib/libeql5_sql.so     android-build/libs/armeabi-v7a/
cp ../../lib/libeql5_svg.so     android-build/libs/armeabi-v7a/
cp $ECL_ANDROID/lib/libecl.so   android-build/libs/armeabi-v7a/
