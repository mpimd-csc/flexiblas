#!/usr/bin/env sh
VER=$1
apt install -y intel-oneapi-mkl-devel-$VER

mkdir -p symbols-$VER
source /opt/intel/oneapi/mkl/${VER}/env/vars.sh

bash ./check_fortran_symbol.sh symbols/blas_symbols.txt symbols-$VER/blas.txt
bash ./check_fortran_symbol.sh symbols/lapack_symbols.txt symbols-$VER/lapack.txt
bash ./check_c_symbol.sh symbols/cblas_symbols.txt symbols-$VER/cblas.txt
bash ./check_c_symbol.sh symbols/lapacke_symbols.txt symbols-$VER/lapacke.txt

bash ./check_fortran_symbol.sh symbols/index64/blas_64_symbols.txt symbols-$VER/blas_64.txt
bash ./check_fortran_symbol.sh symbols/index64/lapack_64_symbols.txt symbols-$VER/lapack_64.txt
bash ./check_c_symbol.sh symbols/index64/cblas_64_symbols.txt symbols-$VER/cblas_64.txt
bash ./check_c_symbol.sh symbols/index64/lapacke_64_symbols.txt symbols-$VER/lapacke_64.txt

