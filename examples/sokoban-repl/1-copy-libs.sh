# ECL, EQL5 libs

mkdir -p android-sources/libs/armeabi-v7a

cp ../../lib/libeql5.so       android-sources/libs/armeabi-v7a/
cp ../../lib/libeql5_quick.so android-sources/libs/armeabi-v7a/
cp $ECL_ANDROID/lib/libecl.so android-sources/libs/armeabi-v7a/

# all prebuilt ECL libs (ASDF etc.)

mkdir -p android-sources/assets/lib/encodings

cp $ECL_ANDROID/lib/ecl-16.1.3/*.asd       android-sources/assets/lib/
cp $ECL_ANDROID/lib/ecl-16.1.3/*.fas       android-sources/assets/lib/
cp $ECL_ANDROID/lib/ecl-16.1.3/*.doc       android-sources/assets/lib/
cp $ECL_ANDROID/lib/ecl-16.1.3/encodings/* android-sources/assets/lib/encodings/

# strip

$ANDROID_NDK_ROOT/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin/arm-linux-androideabi-strip android-sources/assets/lib/*.fas

# a Swank version (with a small patch) that is guaranteed to work on android

mkdir -p android-sources/assets/lib/quicklisp/local-projects

cp -r ../../slime android-sources/assets/lib/quicklisp/local-projects/
