Build GCC for HIP from trunk by typing:
./build-gcc-incremental.sh hip

Requires following libraries to be installed: GMP, MPFR, MPC.
See http://gcc.gnu.org/install/prerequisites.html

The default build installs binaries to /usr/local/gcc-hip-binaries.

To run the compiler, go to /usr/local/gcc-hip-binaries/bin and type:
./hip-gcc -S source.c