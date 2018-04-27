mkdir -p update

# copy app lib
cp android-build/libs/armeabi-v7a/libqtapp.so update/update-sokoban.so

# strip
$ANDROID_NDK_ROOT/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin/arm-linux-androideabi-strip update/*.so
