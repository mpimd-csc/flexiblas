//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2025 Martin Koehler

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
   */



#ifndef FLEXIBLAS_DUMMY_CBLAS_H
#define FLEXIBLAS_DUMMY_CBLAS_H



#ifdef __cplusplus
extern "C" {
#endif

    /*-----------------------------------------------------------------------------
     *  CBLAS Interface
     *-----------------------------------------------------------------------------*/
    void cblas_caxpy(void);
    void cblas_ccopy(void);
    void cblas_cdotc_sub(void);
    void cblas_cdotu_sub(void);
    void cblas_cgbmv(void);
    void cblas_cgemm(void);
    void cblas_cgemv(void);
    void cblas_cgerc(void);
    void cblas_cgeru(void);
    void cblas_chbmv(void);
    void cblas_chemm(void);
    void cblas_chemv(void);
    void cblas_cher2(void);
    void cblas_cher2k(void);
    void cblas_cher(void);
    void cblas_cherk(void);
    void cblas_chpmv(void);
    void cblas_chpr2(void);
    void cblas_chpr(void);
    void cblas_cscal(void);
    void cblas_csscal(void);
    void cblas_cswap(void);
    void cblas_csymm(void);
    void cblas_csyr2k(void);
    void cblas_csyrk(void);
    void cblas_ctbmv(void);
    void cblas_ctbsv(void);
    void cblas_ctpmv(void);
    void cblas_ctpsv(void);
    void cblas_ctrmm(void);
    void cblas_ctrmv(void);
    void cblas_ctrsm(void);
    void cblas_ctrsv(void);
    void cblas_dasum(void);
    void cblas_daxpy(void);
    void cblas_dcopy(void);
    void cblas_ddot(void);
    void cblas_dgbmv(void);
    void cblas_dgemm(void);
    void cblas_dgemv(void);
    void cblas_dger(void);
    void cblas_dnrm2(void);
    void cblas_drot(void);
    void cblas_drotg(void);
    void cblas_drotm(void);
    void cblas_drotmg(void);
    void cblas_dsbmv(void);
    void cblas_dscal(void);
    void cblas_dsdot(void);
    void cblas_dspmv(void);
    void cblas_dspr2(void);
    void cblas_dspr(void);
    void cblas_dswap(void);
    void cblas_dsymm(void);
    void cblas_dsymv(void);
    void cblas_dsyr2(void);
    void cblas_dsyr2k(void);
    void cblas_dsyr(void);
    void cblas_dsyrk(void);
    void cblas_dtbmv(void);
    void cblas_dtbsv(void);
    void cblas_dtpmv(void);
    void cblas_dtpsv(void);
    void cblas_dtrmm(void);
    void cblas_dtrmv(void);
    void cblas_dtrsm(void);
    void cblas_dtrsv(void);
    void cblas_dzasum(void);
    void cblas_dznrm2(void);
    void cblas_icamax(void);
    void cblas_idamax(void);
    void cblas_isamax(void);
    void cblas_izamax(void);
    void cblas_sasum(void);
    void cblas_saxpy(void);
    void cblas_scasum(void);
    void cblas_scnrm2(void);
    void cblas_scopy(void);
    void cblas_sdot(void);
    void cblas_sdsdot(void);
    void cblas_sgbmv(void);
    void cblas_sgemm(void);
    void cblas_sgemv(void);
    void cblas_sger(void);
    void cblas_snrm2(void);
    void cblas_srot(void);
    void cblas_srotg(void);
    void cblas_srotm(void);
    void cblas_srotmg(void);
    void cblas_ssbmv(void);
    void cblas_sscal(void);
    void cblas_sspmv(void);
    void cblas_sspr2(void);
    void cblas_sspr(void);
    void cblas_sswap(void);
    void cblas_ssymm(void);
    void cblas_ssymv(void);
    void cblas_ssyr2(void);
    void cblas_ssyr2k(void);
    void cblas_ssyr(void);
    void cblas_ssyrk(void);
    void cblas_stbmv(void);
    void cblas_stbsv(void);
    void cblas_stpmv(void);
    void cblas_stpsv(void);
    void cblas_strmm(void);
    void cblas_strmv(void);
    void cblas_strsm(void);
    void cblas_strsv(void);
    void cblas_zaxpy(void);
    void cblas_zcopy(void);
    void cblas_zdotc_sub(void);
    void cblas_zdotu_sub(void);
    void cblas_zdscal(void);
    void cblas_zgbmv(void);
    void cblas_zgemm(void);
    void cblas_zgemv(void);
    void cblas_zgerc(void);
    void cblas_zgeru(void);
    void cblas_zhbmv(void);
    void cblas_zhemm(void);
    void cblas_zhemv(void);
    void cblas_zher2(void);
    void cblas_zher2k(void);
    void cblas_zher(void);
    void cblas_zherk(void);
    void cblas_zhpmv(void);
    void cblas_zhpr2(void);
    void cblas_zhpr(void);
    void cblas_zscal(void);
    void cblas_zswap(void);
    void cblas_zsymm(void);
    void cblas_zsyr2k(void);
    void cblas_zsyrk(void);
    void cblas_ztbmv(void);
    void cblas_ztbsv(void);
    void cblas_ztpmv(void);
    void cblas_ztpsv(void);
    void cblas_ztrmm(void);
    void cblas_ztrmv(void);
    void cblas_ztrsm(void);
    void cblas_ztrsv(void);

    void __flexiblas_dummy_function_to_include_all_cblas_symbols(int dummy){
        dummy  = dummy;
        cblas_caxpy();
        cblas_ccopy();
        cblas_cdotc_sub();
        cblas_cdotu_sub();
        cblas_cgbmv();
        cblas_cgemm();
        cblas_cgemv();
        cblas_cgerc();
        cblas_cgeru();
        cblas_chbmv();
        cblas_chemm();
        cblas_chemv();
        cblas_cher2();
        cblas_cher2k();
        cblas_cher();
        cblas_cherk();
        cblas_chpmv();
        cblas_chpr2();
        cblas_chpr();
        cblas_cscal();
        cblas_csscal();
        cblas_cswap();
        cblas_csymm();
        cblas_csyr2k();
        cblas_csyrk();
        cblas_ctbmv();
        cblas_ctbsv();
        cblas_ctpmv();
        cblas_ctpsv();
        cblas_ctrmm();
        cblas_ctrmv();
        cblas_ctrsm();
        cblas_ctrsv();
        cblas_dasum();
        cblas_daxpy();
        cblas_dcopy();
        cblas_ddot();
        cblas_dgbmv();
        cblas_dgemm();
        cblas_dgemv();
        cblas_dger();
        cblas_dnrm2();
        cblas_drot();
        cblas_drotg();
        cblas_drotm();
        cblas_drotmg();
        cblas_dsbmv();
        cblas_dscal();
        cblas_dsdot();
        cblas_dspmv();
        cblas_dspr2();
        cblas_dspr();
        cblas_dswap();
        cblas_dsymm();
        cblas_dsymv();
        cblas_dsyr2();
        cblas_dsyr2k();
        cblas_dsyr();
        cblas_dsyrk();
        cblas_dtbmv();
        cblas_dtbsv();
        cblas_dtpmv();
        cblas_dtpsv();
        cblas_dtrmm();
        cblas_dtrmv();
        cblas_dtrsm();
        cblas_dtrsv();
        cblas_dzasum();
        cblas_dznrm2();
        cblas_icamax();
        cblas_idamax();
        cblas_isamax();
        cblas_izamax();
        cblas_sasum();
        cblas_saxpy();
        cblas_scasum();
        cblas_scnrm2();
        cblas_scopy();
        cblas_sdot();
        cblas_sdsdot();
        cblas_sgbmv();
        cblas_sgemm();
        cblas_sgemv();
        cblas_sger();
        cblas_snrm2();
        cblas_srot();
        cblas_srotg();
        cblas_srotm();
        cblas_srotmg();
        cblas_ssbmv();
        cblas_sscal();
        cblas_sspmv();
        cblas_sspr2();
        cblas_sspr();
        cblas_sswap();
        cblas_ssymm();
        cblas_ssymv();
        cblas_ssyr2();
        cblas_ssyr2k();
        cblas_ssyr();
        cblas_ssyrk();
        cblas_stbmv();
        cblas_stbsv();
        cblas_stpmv();
        cblas_stpsv();
        cblas_strmm();
        cblas_strmv();
        cblas_strsm();
        cblas_strsv();
        cblas_zaxpy();
        cblas_zcopy();
        cblas_zdotc_sub();
        cblas_zdotu_sub();
        cblas_zdscal();
        cblas_zgbmv();
        cblas_zgemm();
        cblas_zgemv();
        cblas_zgerc();
        cblas_zgeru();
        cblas_zhbmv();
        cblas_zhemm();
        cblas_zhemv();
        cblas_zher2();
        cblas_zher2k();
        cblas_zher();
        cblas_zherk();
        cblas_zhpmv();
        cblas_zhpr2();
        cblas_zhpr();
        cblas_zscal();
        cblas_zswap();
        cblas_zsymm();
        cblas_zsyr2k();
        cblas_zsyrk();
        cblas_ztbmv();
        cblas_ztbsv();
        cblas_ztpmv();
        cblas_ztpsv();
        cblas_ztrmm();
        cblas_ztrmv();
        cblas_ztrsm();
        cblas_ztrsv();

    }




#ifdef __cplusplus
}
#endif

#endif /* end of include guard: FLEXIBLAS_DUMMY_CBLAS_H */
