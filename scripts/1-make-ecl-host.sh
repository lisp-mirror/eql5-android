# build the host ECL, which will then be used
# to build the cross-compiled Android version

./configure ABI=32 CFLAGS="-m32 -g -O2" LDFLAGS="-m32 -g -O2" --prefix=`pwd`/ecl-android-host --disable-longdouble
make
make install
rm -r build
