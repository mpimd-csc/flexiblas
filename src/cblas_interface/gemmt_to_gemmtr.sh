#!/usr/bin/env sh
sed -e 's/gemmt/gemmtr/g' -e 's/GEMMT/GEMMTR/g' cblas_sgemmt.c > cblas_sgemmtr.c
sed -e 's/gemmt/gemmtr/g' -e 's/GEMMT/GEMMTR/g' cblas_dgemmt.c > cblas_dgemmtr.c
sed -e 's/gemmt/gemmtr/g' -e 's/GEMMT/GEMMTR/g' cblas_cgemmt.c > cblas_cgemmtr.c
sed -e 's/gemmt/gemmtr/g' -e 's/GEMMT/GEMMTR/g' cblas_zgemmt.c > cblas_zgemmtr.c

