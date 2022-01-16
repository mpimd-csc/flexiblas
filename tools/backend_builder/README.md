FlexiBLAS Backend Builder
=========================

This directory contains a small CMake helper to build new BLAS and LAPACK for
FlexiBLAS. The tool requires a FlexiBLAS installation with a version larger than
3.0.4. If FlexiBLAS is installed from a package manager, the development
packages must also be installed. Furthermore, it requires CMake >= 3.5

Usage
-----

A new BLAS interface is built in the following way:
```shell
mkdir build
cd build
cmake ../  -DBLAS_NAME="NameOfTheBlasLibrary" \
           -DBLAS_LIBRARY="LibrariesToLink" \
           -DBLAS_LINKER_FLAGS="AdditionalLinkerFlags"
make
make install
```
The `BLAS_LINKER_FLAGS` argument is optional.

For example you want to install a custom verion of OpenBLAS, compiled in
`${HOME}/software/OpenBLAS`, you perform the following steps:
```shell
mkdir build.myopenblas
cd build.myopenblas
cmake ../  -DBLAS_NAME="MyOpenBLAS" \
           -DBLAS_LIBRARY="${HOME}/software/OpenBLAS/libopenblas.a;gfortran;gomp;pthread;m"
make
make install

```

In this way, the Intel MKL library can also be added easily:
```shell
mkdir build.mkl
cd build.mkl
cmake ../ -DBLAS_NAME="MyMKL" \
          -DBLAS_LIBRARY="-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl"
```

