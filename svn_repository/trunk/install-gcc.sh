#!/bin/bash

export TARGET=$1
export PREFIX=/usr/local/gcc-$TARGET-binaries/

cd ./build-gcc-$TARGET
export PATH=$PATH:$PREFIX/bin

echo "Installing GCC"
make install-gcc --quiet
