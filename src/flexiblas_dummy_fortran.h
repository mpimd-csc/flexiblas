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




#ifndef FLEXIBLAS_DUMMY_FORTRAN_H
#define FLEXIBLAS_DUMMY_FORTRAN_H



#ifdef __cplusplus
extern "C" {
#endif

/*-----------------------------------------------------------------------------
 *  Dummy Declaration.
 *----------------------------------------------------------------------------*/
void	caxpy_(void);
void	ccopy_(void);
void	cdotc_(void);
void	cdotu_(void);
void	cgbmv_(void);
void	cgemm_(void);
void	cgemv_(void);
void	cgerc_(void);
void	cgeru_(void);
void	chbmv_(void);
void	chemm_(void);
void	chemv_(void);
void	cher2_(void);
void	cher2k_(void);
void	cher_(void);
void	cherk_(void);
void	chpmv_(void);
void	chpr2_(void);
void	chpr_(void);
void	crotg_(void);
void	cscal_(void);
void	csrot_(void);
void	csscal_(void);
void	cswap_(void);
void	csymm_(void);
void	csyr2k_(void);
void	csyrk_(void);
void	ctbmv_(void);
void	ctbsv_(void);
void	ctpmv_(void);
void	ctpsv_(void);
void	ctrmm_(void);
void	ctrmv_(void);
void	ctrsm_(void);
void	ctrsv_(void);
void	dasum_(void);
void	daxpy_(void);
void	dcopy_(void);
void	ddot_(void);
void	dgbmv_(void);
void	dgemm_(void);
void	dgemv_(void);
void	dger_(void);
void	dnrm2_(void);
void	drot_(void);
void	drotg_(void);
void	drotm_(void);
void	drotmg_(void);
void	dsbmv_(void);
void	dscal_(void);
void	dsdot_(void);
void	dspmv_(void);
void	dspr2_(void);
void	dspr_(void);
void	dswap_(void);
void	dsymm_(void);
void	dsymv_(void);
void	dsyr2_(void);
void	dsyr2k_(void);
void	dsyr_(void);
void	dsyrk_(void);
void	dtbmv_(void);
void	dtbsv_(void);
void	dtpmv_(void);
void	dtpsv_(void);
void	dtrmm_(void);
void	dtrmv_(void);
void	dtrsm_(void);
void	dtrsv_(void);
void	dzasum_(void);
void	dznrm2_(void);
void	icamax_(void);
void	idamax_(void);
void	isamax_(void);
void	izamax_(void);
void	sasum_(void);
void	saxpy_(void);
#ifndef SCABS_MISSING
void	scabs1_(void);
void	dcabs1_(void);
#endif
void	scasum_(void);
void	scnrm2_(void);
void	scopy_(void);
void	sdot_(void);
void	sdsdot_(void);
void	sgbmv_(void);
void	sgemm_(void);
void	sgemv_(void);
void	sger_(void);
void	snrm2_(void);
void	srot_(void);
void	srotg_(void);
void	srotm_(void);
void	srotmg_(void);
void	ssbmv_(void);
void	sscal_(void);
void	sspmv_(void);
void	sspr2_(void);
void	sspr_(void);
void	sswap_(void);
void	ssymm_(void);
void	ssymv_(void);
void	ssyr2_(void);
void	ssyr2k_(void);
void	ssyr_(void);
void	ssyrk_(void);
void	stbmv_(void);
void	stbsv_(void);
void	stpmv_(void);
void	stpsv_(void);
void	strmm_(void);
void	strmv_(void);
void	strsm_(void);
void	strsv_(void);
void	zaxpy_(void);
void	zcopy_(void);
void	zdotc_(void);
void	zdotu_(void);
void	zdrot_(void);
void	zdscal_(void);
void	zgbmv_(void);
void	zgemm_(void);
void	zgemv_(void);
void	zgerc_(void);
void	zgeru_(void);
void	zhbmv_(void);
void	zhemm_(void);
void	zhemv_(void);
void	zher2_(void);
void	zher2k_(void);
void	zher_(void);
void	zherk_(void);
void	zhpmv_(void);
void	zhpr2_(void);
void	zhpr_(void);
void	zrotg_(void);
void	zscal_(void);
void	zswap_(void);
void	zsymm_(void);
void	zsyr2k_(void);
void	zsyrk_(void);
void	ztbmv_(void);
void	ztbsv_(void);
void	ztpmv_(void);
void	ztpsv_(void);
void	ztrmm_(void);
void	ztrmv_(void);
void	ztrsm_(void);
void	ztrsv_(void);


void __flexiblas_dummy_function_to_include_all_blas_symbols(int dummy){
    dummy = dummy;
	caxpy_();
	ccopy_();
	cdotc_();
	cdotu_();
	cgbmv_();
	cgemm_();
	cgemv_();
	cgerc_();
	cgeru_();
	chbmv_();
	chemm_();
	chemv_();
	cher2_();
	cher2k_();
	cher_();
	cherk_();
	chpmv_();
	chpr2_();
	chpr_();
	crotg_();
	cscal_();
	csrot_();
	csscal_();
	cswap_();
	csymm_();
	csyr2k_();
	csyrk_();
	ctbmv_();
	ctbsv_();
	ctpmv_();
	ctpsv_();
	ctrmm_();
	ctrmv_();
	ctrsm_();
	ctrsv_();
	dasum_();
	daxpy_();
	dcopy_();
	ddot_();
	dgbmv_();
	dgemm_();
	dgemv_();
	dger_();
	dnrm2_();
	drot_();
	drotg_();
	drotm_();
	drotmg_();
	dsbmv_();
	dscal_();
	dsdot_();
	dspmv_();
	dspr2_();
	dspr_();
	dswap_();
	dsymm_();
	dsymv_();
	dsyr2_();
	dsyr2k_();
	dsyr_();
	dsyrk_();
	dtbmv_();
	dtbsv_();
	dtpmv_();
	dtpsv_();
	dtrmm_();
	dtrmv_();
	dtrsm_();
	dtrsv_();
	dzasum_();
	dznrm2_();
	icamax_();
	idamax_();
	isamax_();
	izamax_();
	sasum_();
	saxpy_();
#ifndef SCABS_MISSING
	scabs1_();
	dcabs1_();
#endif
	scasum_();
	scnrm2_();
	scopy_();
	sdot_();
	sdsdot_();
	sgbmv_();
	sgemm_();
	sgemv_();
	sger_();
	snrm2_();
	srot_();
	srotg_();
	srotm_();
	srotmg_();
	ssbmv_();
	sscal_();
	sspmv_();
	sspr2_();
	sspr_();
	sswap_();
	ssymm_();
	ssymv_();
	ssyr2_();
	ssyr2k_();
	ssyr_();
	ssyrk_();
	stbmv_();
	stbsv_();
	stpmv_();
	stpsv_();
	strmm_();
	strmv_();
	strsm_();
	strsv_();
	zaxpy_();
	zcopy_();
	zdotc_();
	zdotu_();
	zdrot_();
	zdscal_();
	zgbmv_();
	zgemm_();
	zgemv_();
	zgerc_();
	zgeru_();
	zhbmv_();
	zhemm_();
	zhemv_();
	zher2_();
	zher2k_();
	zher_();
	zherk_();
	zhpmv_();
	zhpr2_();
	zhpr_();
	zrotg_();
	zscal_();
	zswap_();
	zsymm_();
	zsyr2k_();
	zsyrk_();
	ztbmv_();
	ztbsv_();
	ztpmv_();
	ztpsv_();
	ztrmm_();
	ztrmv_();
	ztrsm_();
	ztrsv_();
}






#ifdef __cplusplus
}
#endif

#endif /* end of include guard: FLEXIBLAS_DUMMY_FORTRAN_H */

