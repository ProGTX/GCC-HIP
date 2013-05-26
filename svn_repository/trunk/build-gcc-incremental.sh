#!/bin/bash

export PREFIX=/local/cross
export TARGET=$1

cd /src/build-gcc-$TARGET
export PATH=$PATH:$PREFIX/bin

rm ./gcc/gtype.state

echo "Configuring GCC"
../gcc/configure -C --quiet --target=$TARGET --prefix=$PREFIX --disable-nls --enable-languages=c --without-headers

echo "Compiling GCC"
make -j4 all-gcc --quiet

echo "Installing GCC"
make install-gcc --quiet
