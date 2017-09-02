# use the previously built host ECL to build the android version

export NDK_PATH=${ANDROID_NDK_ROOT}
export SYSROOT=${NDK_PATH}/platforms/android-16/arch-arm
export PATH=${NDK_PATH}/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin:$PATH
export ECL_TO_RUN=`pwd`/ecl-android-host/bin/ecl

export LDFLAGS="--sysroot=${SYSROOT}"
export CPPFLAGS="--sysroot=${SYSROOT}"
./configure --host=arm-linux-androideabi \
            --enable-boehm=included \
            --prefix=`pwd`/ecl-android \
            --with-cross-config=`pwd`/src/util/android.cross_config
make
make install
