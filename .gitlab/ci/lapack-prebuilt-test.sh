#!/bin/bash
set -x
set -e

if [ "X${API_VERSION}" = "X" ]
then
	LAPACK_API=""
else
	LAPACK_API="-DLAPACK_API_VERSION=${API_VERSION}"

fi


mkdir build
cd build
cmake ../ -DDEV=ON -DDEBUG=ON -DSYS_BLAS_LIBRARY=${BASE}/${VERSION}/lib/libblas.a -DSYS_LAPACK_LIBRARY=${BASE}/${VERSION}/lib/liblapack.a ${LAPACK_API}\
    -DCMAKE_Fortran_FLAGS="-fallow-argument-mismatch"
make
make test

