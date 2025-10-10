#!/usr/bin/env sh
X=$(pwd)
for i in lapacke*
do
    cd "${X}"
    cd "${i}"

    mkdir -p src/extended
    git mv src/lapacke_cgbrfsx.c src/extended/
    git mv src/lapacke_cporfsx.c src/extended/
    git mv src/lapacke_dgerfsx.c src/extended/
    git mv src/lapacke_sgbrfsx.c src/extended/
    git mv src/lapacke_ssyrfsx.c src/extended/
    git mv src/lapacke_zherfsx.c src/extended/
    git mv src/lapacke_cgbrfsx_work.c src/extended/
    git mv src/lapacke_cporfsx_work.c src/extended/
    git mv src/lapacke_dgerfsx_work.c src/extended/
    git mv src/lapacke_sgbrfsx_work.c src/extended/
    git mv src/lapacke_ssyrfsx_work.c src/extended/
    git mv src/lapacke_zherfsx_work.c src/extended/
    git mv src/lapacke_cgerfsx.c src/extended/
    git mv src/lapacke_csyrfsx.c src/extended/
    git mv src/lapacke_dporfsx.c src/extended/
    git mv src/lapacke_sgerfsx.c src/extended/
    git mv src/lapacke_zgbrfsx.c src/extended/
    git mv src/lapacke_zporfsx.c src/extended/
    git mv src/lapacke_cgerfsx_work.c src/extended/
    git mv src/lapacke_csyrfsx_work.c src/extended/
    git mv src/lapacke_dporfsx_work.c src/extended/
    git mv src/lapacke_sgerfsx_work.c src/extended/
    git mv src/lapacke_zgbrfsx_work.c src/extended/
    git mv src/lapacke_zporfsx_work.c src/extended/
    git mv src/lapacke_cherfsx.c src/extended/
    git mv src/lapacke_dgbrfsx.c src/extended/
    git mv src/lapacke_dsyrfsx.c src/extended/
    git mv src/lapacke_sporfsx.c src/extended/
    git mv src/lapacke_zgerfsx.c src/extended/
    git mv src/lapacke_zsyrfsx.c src/extended/
    git mv src/lapacke_cherfsx_work.c src/extended/
    git mv src/lapacke_dgbrfsx_work.c src/extended/
    git mv src/lapacke_dsyrfsx_work.c src/extended/
    git mv src/lapacke_sporfsx_work.c src/extended/
    git mv src/lapacke_zgerfsx_work.c src/extended/
    git mv src/lapacke_zsyrfsx_work.c src/extended/
    git mv src/lapacke_cgbsvxx.c src/extended/
    git mv src/lapacke_cposvxx.c src/extended/
    git mv src/lapacke_dgesvxx.c src/extended/
    git mv src/lapacke_sgbsvxx.c src/extended/
    git mv src/lapacke_ssysvxx.c src/extended/
    git mv src/lapacke_zhesvxx.c src/extended/
    git mv src/lapacke_cgbsvxx_work.c src/extended/
    git mv src/lapacke_cposvxx_work.c src/extended/
    git mv src/lapacke_dgesvxx_work.c src/extended/
    git mv src/lapacke_sgbsvxx_work.c src/extended/
    git mv src/lapacke_ssysvxx_work.c src/extended/
    git mv src/lapacke_zhesvxx_work.c src/extended/
    git mv src/lapacke_cgesvxx.c src/extended/
    git mv src/lapacke_csysvxx.c src/extended/
    git mv src/lapacke_dposvxx.c src/extended/
    git mv src/lapacke_sgesvxx.c src/extended/
    git mv src/lapacke_zgbsvxx.c src/extended/
    git mv src/lapacke_zposvxx.c src/extended/
    git mv src/lapacke_cgesvxx_work.c src/extended/
    git mv src/lapacke_csysvxx_work.c src/extended/
    git mv src/lapacke_dposvxx_work.c src/extended/
    git mv src/lapacke_sgesvxx_work.c src/extended/
    git mv src/lapacke_zgbsvxx_work.c src/extended/
    git mv src/lapacke_zposvxx_work.c src/extended/
    git mv src/lapacke_chesvxx.c src/extended/
    git mv src/lapacke_dgbsvxx.c src/extended/
    git mv src/lapacke_dsysvxx.c src/extended/
    git mv src/lapacke_sposvxx.c src/extended/
    git mv src/lapacke_zgesvxx.c src/extended/
    git mv src/lapacke_zsysvxx.c src/extended/
    git mv src/lapacke_chesvxx_work.c src/extended/
    git mv src/lapacke_dgbsvxx_work.c src/extended/
    git mv src/lapacke_dsysvxx_work.c src/extended/
    git mv src/lapacke_sposvxx_work.c src/extended/
    git mv src/lapacke_zgesvxx_work.c src/extended/
    git mv src/lapacke_zsysvxx_work.c src/extended/
done
