# ECL, EQL5 libs

mkdir android-build
mkdir android-build/libs
mkdir android-build/libs/armeabi-v7a

cp ../../lib/libeql5.so            android-build/libs/armeabi-v7a/
cp ../../lib/libeql5_quick.so      android-build/libs/armeabi-v7a/
cp ../../lib/libeql5_multimedia.so android-build/libs/armeabi-v7a/
cp ../../lib/libeql5_network.so    android-build/libs/armeabi-v7a/
cp ../../lib/libeql5_sql.so        android-build/libs/armeabi-v7a/
cp ../../lib/libeql5_svg.so        android-build/libs/armeabi-v7a/
cp $ECL_ANDROID/lib/libecl.so      android-build/libs/armeabi-v7a/

# all prebuilt ECL libs (ASDF etc.)

mkdir android-build/assets
mkdir android-build/assets/lib
mkdir android-build/assets/lib/encodings

cp $ECL_ANDROID/lib/ecl-16.1.3/*.asd       android-build/assets/lib/
cp $ECL_ANDROID/lib/ecl-16.1.3/*.fas       android-build/assets/lib/
cp $ECL_ANDROID/lib/ecl-16.1.3/*.doc       android-build/assets/lib/
cp $ECL_ANDROID/lib/ecl-16.1.3/encodings/* android-build/assets/lib/encodings/

# strip

$ANDROID_NDK_ROOT/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin/arm-linux-androideabi-strip android-build/assets/lib/*.fas

# a Swank version (with a small patch) that is guaranteed to work on android

mkdir android-build/assets/lib/quicklisp
mkdir android-build/assets/lib/quicklisp/local-projects

cp -r ../../slime android-build/assets/lib/quicklisp/local-projects/
