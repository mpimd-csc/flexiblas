/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Linking FlexiBLAS statically or dynamically with other modules is making a
 * combined work based on FlexiBLAS. Thus, the terms and conditions of the GNU
 * General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2023
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
