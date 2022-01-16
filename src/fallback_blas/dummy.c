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
 * Copyright (C) Martin Koehler, 2013-2022
 */




#include "flexiblas_config.h"
#include "fortran_mangle.h"



void FC_GLOBAL(caxpy,CAXPY)(void);
void FC_GLOBAL(ccopy,CCOPY)(void);
void FC_GLOBAL(cdotc,CDOTC)(void);
void FC_GLOBAL(cdotu,CDOTU)(void);
void FC_GLOBAL(cgbmv,CGBMV)(void);
void FC_GLOBAL(cgemm,CGEMM)(void);
void FC_GLOBAL(cgemv,CGEMV)(void);
void FC_GLOBAL(cgerc,CGERC)(void);
void FC_GLOBAL(cgeru,CGERU)(void);
void FC_GLOBAL(chbmv,CHBMV)(void);
void FC_GLOBAL(chemm,CHEMM)(void);
void FC_GLOBAL(chemv,CHEMV)(void);
void FC_GLOBAL(cher2,CHER2)(void);
void FC_GLOBAL(cher2k,CHER2K)(void);
void FC_GLOBAL(cher,CHER)(void);
void FC_GLOBAL(cherk,CHERK)(void);
void FC_GLOBAL(chpmv,CHPMV)(void);
void FC_GLOBAL(chpr2,CHPR2)(void);
void FC_GLOBAL(chpr,CHPR)(void);
void FC_GLOBAL(crotg,CROTG)(void);
void FC_GLOBAL(cscal,CSCAL)(void);
void FC_GLOBAL(csrot,CSROT)(void);
void FC_GLOBAL(csscal,CSSCAL)(void);
void FC_GLOBAL(cswap,CSWAP)(void);
void FC_GLOBAL(csymm,CSYMM)(void);
void FC_GLOBAL(csyr2k,CSYR2K)(void);
void FC_GLOBAL(csyrk,CSYRK)(void);
void FC_GLOBAL(ctbmv,CTBMV)(void);
void FC_GLOBAL(ctbsv,CTBSV)(void);
void FC_GLOBAL(ctpmv,CTPMV)(void);
void FC_GLOBAL(ctpsv,CTPSV)(void);
void FC_GLOBAL(ctrmm,CTRMM)(void);
void FC_GLOBAL(ctrmv,CTRMV)(void);
void FC_GLOBAL(ctrsm,CTRSM)(void);
void FC_GLOBAL(ctrsv,CTRSV)(void);
void FC_GLOBAL(dasum,DASUM)(void);
void FC_GLOBAL(daxpy,DAXPY)(void);
void FC_GLOBAL(dcopy,DCOPY)(void);
void FC_GLOBAL(ddot,DDOT)(void);
void FC_GLOBAL(dgbmv,DGBMV)(void);
void FC_GLOBAL(dgemm,DGEMM)(void);
void FC_GLOBAL(dgemv,DGEMV)(void);
void FC_GLOBAL(dger,DGER)(void);
void FC_GLOBAL(dnrm2,DNRM2)(void);
void FC_GLOBAL(drot,DROT)(void);
void FC_GLOBAL(drotg,DROTG)(void);
void FC_GLOBAL(drotm,DROTM)(void);
void FC_GLOBAL(drotmg,DROTMG)(void);
void FC_GLOBAL(dsbmv,DSBMV)(void);
void FC_GLOBAL(dscal,DSCAL)(void);
void FC_GLOBAL(dsdot,DSDOT)(void);
void FC_GLOBAL(dspmv,DSPMV)(void);
void FC_GLOBAL(dspr2,DSPR2)(void);
void FC_GLOBAL(dspr,DSPR)(void);
void FC_GLOBAL(dswap,DSWAP)(void);
void FC_GLOBAL(dsymm,DSYMM)(void);
void FC_GLOBAL(dsymv,DSYMV)(void);
void FC_GLOBAL(dsyr2,DSYR2)(void);
void FC_GLOBAL(dsyr2k,DSYR2K)(void);
void FC_GLOBAL(dsyr,DSYR)(void);
void FC_GLOBAL(dsyrk,DSYRK)(void);
void FC_GLOBAL(dtbmv,DTBMV)(void);
void FC_GLOBAL(dtbsv,DTBSV)(void);
void FC_GLOBAL(dtpmv,DTPMV)(void);
void FC_GLOBAL(dtpsv,DTPSV)(void);
void FC_GLOBAL(dtrmm,DTRMM)(void);
void FC_GLOBAL(dtrmv,DTRMV)(void);
void FC_GLOBAL(dtrsm,DTRSM)(void);
void FC_GLOBAL(dtrsv,DTRSV)(void);
void FC_GLOBAL(dzasum,DZASUM)(void);
void FC_GLOBAL(dznrm2,DZNRM2)(void);
void FC_GLOBAL(icamax,ICAMAX)(void);
void FC_GLOBAL(idamax,IDAMAX)(void);
void FC_GLOBAL(isamax,ISAMAX)(void);
void FC_GLOBAL(izamax,IZAMAX)(void);
void FC_GLOBAL(sasum,SASUM)(void);
void FC_GLOBAL(saxpy,SAXPY)(void);
void FC_GLOBAL(scabs1,SCABS1)(void);
void FC_GLOBAL(dcabs1,DCABS1)(void);
void FC_GLOBAL(scasum,SCASUM)(void);
void FC_GLOBAL(scnrm2,SCNRM2)(void);
void FC_GLOBAL(scopy,SCOPY)(void);
void FC_GLOBAL(sdot,SDOT)(void);
void FC_GLOBAL(sdsdot,SDSDOT)(void);
void FC_GLOBAL(sgbmv,SGBMV)(void);
void FC_GLOBAL(sgemm,SGEMM)(void);
void FC_GLOBAL(sgemv,SGEMV)(void);
void FC_GLOBAL(sger,SGER)(void);
void FC_GLOBAL(snrm2,SNRM2)(void);
void FC_GLOBAL(srot,SROT)(void);
void FC_GLOBAL(srotg,SROTG)(void);
void FC_GLOBAL(srotm,SROTM)(void);
void FC_GLOBAL(srotmg,SROTMG)(void);
void FC_GLOBAL(ssbmv,SSBMV)(void);
void FC_GLOBAL(sscal,SSCAL)(void);
void FC_GLOBAL(sspmv,SSPMV)(void);
void FC_GLOBAL(sspr2,SSPR2)(void);
void FC_GLOBAL(sspr,SSPR)(void);
void FC_GLOBAL(sswap,SSWAP)(void);
void FC_GLOBAL(ssymm,SSYMM)(void);
void FC_GLOBAL(ssymv,SSYMV)(void);
void FC_GLOBAL(ssyr2,SSYR2)(void);
void FC_GLOBAL(ssyr2k,SSYR2K)(void);
void FC_GLOBAL(ssyr,SSYR)(void);
void FC_GLOBAL(ssyrk,SSYRK)(void);
void FC_GLOBAL(stbmv,STBMV)(void);
void FC_GLOBAL(stbsv,STBSV)(void);
void FC_GLOBAL(stpmv,STPMV)(void);
void FC_GLOBAL(stpsv,STPSV)(void);
void FC_GLOBAL(strmm,STRMM)(void);
void FC_GLOBAL(strmv,STRMV)(void);
void FC_GLOBAL(strsm,STRSM)(void);
void FC_GLOBAL(strsv,STRSV)(void);
void FC_GLOBAL(zaxpy,ZAXPY)(void);
void FC_GLOBAL(zcopy,ZCOPY)(void);
void FC_GLOBAL(zdotc,ZDOTC)(void);
void FC_GLOBAL(zdotu,ZDOTU)(void);
void FC_GLOBAL(zdrot,ZDROT)(void);
void FC_GLOBAL(zdscal,ZDSCAL)(void);
void FC_GLOBAL(zgbmv,ZGBMV)(void);
void FC_GLOBAL(zgemm,ZGEMM)(void);
void FC_GLOBAL(zgemv,ZGEMV)(void);
void FC_GLOBAL(zgerc,ZGERC)(void);
void FC_GLOBAL(zgeru,ZGERU)(void);
void FC_GLOBAL(zhbmv,ZHBMV)(void);
void FC_GLOBAL(zhemm,ZHEMM)(void);
void FC_GLOBAL(zhemv,ZHEMV)(void);
void FC_GLOBAL(zher2,ZHER2)(void);
void FC_GLOBAL(zher2k,ZHER2K)(void);
void FC_GLOBAL(zher,ZHER)(void);
void FC_GLOBAL(zherk,ZHERK)(void);
void FC_GLOBAL(zhpmv,ZHPMV)(void);
void FC_GLOBAL(zhpr2,ZHPR2)(void);
void FC_GLOBAL(zhpr,ZHPR)(void);
void FC_GLOBAL(zrotg,ZROTG)(void);
void FC_GLOBAL(zscal,ZSCAL)(void);
void FC_GLOBAL(zswap,ZSWAP)(void);
void FC_GLOBAL(zsymm,ZSYMM)(void);
void FC_GLOBAL(zsyr2k,ZSYR2K)(void);
void FC_GLOBAL(zsyrk,ZSYRK)(void);
void FC_GLOBAL(ztbmv,ZTBMV)(void);
void FC_GLOBAL(ztbsv,ZTBSV)(void);
void FC_GLOBAL(ztpmv,ZTPMV)(void);
void FC_GLOBAL(ztpsv,ZTPSV)(void);
void FC_GLOBAL(ztrmm,ZTRMM)(void);
void FC_GLOBAL(ztrmv,ZTRMV)(void);
void FC_GLOBAL(ztrsm,ZTRSM)(void);
void FC_GLOBAL(ztrsv,ZTRSV)(void);


HIDDEN void __flexiblas_dummy_function_to_include_all_blas_symbols(int dummy){
    dummy = dummy;
 FC_GLOBAL(caxpy,CAXPY)();
 FC_GLOBAL(ccopy,CCOPY)();
 FC_GLOBAL(cdotc,CDOTC)();
 FC_GLOBAL(cdotu,CDOTU)();
 FC_GLOBAL(cgbmv,CGBMV)();
 FC_GLOBAL(cgemm,CGEMM)();
 FC_GLOBAL(cgemv,CGEMV)();
 FC_GLOBAL(cgerc,CGERC)();
 FC_GLOBAL(cgeru,CGERU)();
 FC_GLOBAL(chbmv,CHBMV)();
 FC_GLOBAL(chemm,CHEMM)();
 FC_GLOBAL(chemv,CHEMV)();
 FC_GLOBAL(cher2,CHER2)();
 FC_GLOBAL(cher2k,CHER2K)();
 FC_GLOBAL(cher,CHER)();
 FC_GLOBAL(cherk,CHERK)();
 FC_GLOBAL(chpmv,CHPMV)();
 FC_GLOBAL(chpr2,CHPR2)();
 FC_GLOBAL(chpr,CHPR)();
 FC_GLOBAL(crotg,CROTG)();
 FC_GLOBAL(cscal,CSCAL)();
 FC_GLOBAL(csrot,CSROT)();
 FC_GLOBAL(csscal,CSSCAL)();
 FC_GLOBAL(cswap,CSWAP)();
 FC_GLOBAL(csymm,CSYMM)();
 FC_GLOBAL(csyr2k,CSYR2K)();
 FC_GLOBAL(csyrk,CSYRK)();
 FC_GLOBAL(ctbmv,CTBMV)();
 FC_GLOBAL(ctbsv,CTBSV)();
 FC_GLOBAL(ctpmv,CTPMV)();
 FC_GLOBAL(ctpsv,CTPSV)();
 FC_GLOBAL(ctrmm,CTRMM)();
 FC_GLOBAL(ctrmv,CTRMV)();
 FC_GLOBAL(ctrsm,CTRSM)();
 FC_GLOBAL(ctrsv,CTRSV)();
 FC_GLOBAL(dasum,DASUM)();
 FC_GLOBAL(daxpy,DAXPY)();
 FC_GLOBAL(dcopy,DCOPY)();
 FC_GLOBAL(ddot,DDOT)();
 FC_GLOBAL(dgbmv,DGBMV)();
 FC_GLOBAL(dgemm,DGEMM)();
 FC_GLOBAL(dgemv,DGEMV)();
 FC_GLOBAL(dger,DGER)();
 FC_GLOBAL(dnrm2,DNRM2)();
 FC_GLOBAL(drot,DROT)();
 FC_GLOBAL(drotg,DROTG)();
 FC_GLOBAL(drotm,DROTM)();
 FC_GLOBAL(drotmg,DROTMG)();
 FC_GLOBAL(dsbmv,DSBMV)();
 FC_GLOBAL(dscal,DSCAL)();
 FC_GLOBAL(dsdot,DSDOT)();
 FC_GLOBAL(dspmv,DSPMV)();
 FC_GLOBAL(dspr2,DSPR2)();
 FC_GLOBAL(dspr,DSPR)();
 FC_GLOBAL(dswap,DSWAP)();
 FC_GLOBAL(dsymm,DSYMM)();
 FC_GLOBAL(dsymv,DSYMV)();
 FC_GLOBAL(dsyr2,DSYR2)();
 FC_GLOBAL(dsyr2k,DSYR2K)();
 FC_GLOBAL(dsyr,DSYR)();
 FC_GLOBAL(dsyrk,DSYRK)();
 FC_GLOBAL(dtbmv,DTBMV)();
 FC_GLOBAL(dtbsv,DTBSV)();
 FC_GLOBAL(dtpmv,DTPMV)();
 FC_GLOBAL(dtpsv,DTPSV)();
 FC_GLOBAL(dtrmm,DTRMM)();
 FC_GLOBAL(dtrmv,DTRMV)();
 FC_GLOBAL(dtrsm,DTRSM)();
 FC_GLOBAL(dtrsv,DTRSV)();
 FC_GLOBAL(dzasum,DZASUM)();
 FC_GLOBAL(dznrm2,DZNRM2)();
 FC_GLOBAL(icamax,ICAMAX)();
 FC_GLOBAL(idamax,IDAMAX)();
 FC_GLOBAL(isamax,ISAMAX)();
 FC_GLOBAL(izamax,IZAMAX)();
 FC_GLOBAL(sasum,SASUM)();
 FC_GLOBAL(saxpy,SAXPY)();
 FC_GLOBAL(scabs1,SCABS1)();
 FC_GLOBAL(dcabs1,DCABS1)();
 FC_GLOBAL(scasum,SCASUM)();
 FC_GLOBAL(scnrm2,SCNRM2)();
 FC_GLOBAL(scopy,SCOPY)();
 FC_GLOBAL(sdot,SDOT)();
 FC_GLOBAL(sdsdot,SDSDOT)();
 FC_GLOBAL(sgbmv,SGBMV)();
 FC_GLOBAL(sgemm,SGEMM)();
 FC_GLOBAL(sgemv,SGEMV)();
 FC_GLOBAL(sger,SGER)();
 FC_GLOBAL(snrm2,SNRM2)();
 FC_GLOBAL(srot,SROT)();
 FC_GLOBAL(srotg,SROTG)();
 FC_GLOBAL(srotm,SROTM)();
 FC_GLOBAL(srotmg,SROTMG)();
 FC_GLOBAL(ssbmv,SSBMV)();
 FC_GLOBAL(sscal,SSCAL)();
 FC_GLOBAL(sspmv,SSPMV)();
 FC_GLOBAL(sspr2,SSPR2)();
 FC_GLOBAL(sspr,SSPR)();
 FC_GLOBAL(sswap,SSWAP)();
 FC_GLOBAL(ssymm,SSYMM)();
 FC_GLOBAL(ssymv,SSYMV)();
 FC_GLOBAL(ssyr2,SSYR2)();
 FC_GLOBAL(ssyr2k,SSYR2K)();
 FC_GLOBAL(ssyr,SSYR)();
 FC_GLOBAL(ssyrk,SSYRK)();
 FC_GLOBAL(stbmv,STBMV)();
 FC_GLOBAL(stbsv,STBSV)();
 FC_GLOBAL(stpmv,STPMV)();
 FC_GLOBAL(stpsv,STPSV)();
 FC_GLOBAL(strmm,STRMM)();
 FC_GLOBAL(strmv,STRMV)();
 FC_GLOBAL(strsm,STRSM)();
 FC_GLOBAL(strsv,STRSV)();
 FC_GLOBAL(zaxpy,ZAXPY)();
 FC_GLOBAL(zcopy,ZCOPY)();
 FC_GLOBAL(zdotc,ZDOTC)();
 FC_GLOBAL(zdotu,ZDOTU)();
 FC_GLOBAL(zdrot,ZDROT)();
 FC_GLOBAL(zdscal,ZDSCAL)();
 FC_GLOBAL(zgbmv,ZGBMV)();
 FC_GLOBAL(zgemm,ZGEMM)();
 FC_GLOBAL(zgemv,ZGEMV)();
 FC_GLOBAL(zgerc,ZGERC)();
 FC_GLOBAL(zgeru,ZGERU)();
 FC_GLOBAL(zhbmv,ZHBMV)();
 FC_GLOBAL(zhemm,ZHEMM)();
 FC_GLOBAL(zhemv,ZHEMV)();
 FC_GLOBAL(zher2,ZHER2)();
 FC_GLOBAL(zher2k,ZHER2K)();
 FC_GLOBAL(zher,ZHER)();
 FC_GLOBAL(zherk,ZHERK)();
 FC_GLOBAL(zhpmv,ZHPMV)();
 FC_GLOBAL(zhpr2,ZHPR2)();
 FC_GLOBAL(zhpr,ZHPR)();
 FC_GLOBAL(zrotg,ZROTG)();
 FC_GLOBAL(zscal,ZSCAL)();
 FC_GLOBAL(zswap,ZSWAP)();
 FC_GLOBAL(zsymm,ZSYMM)();
 FC_GLOBAL(zsyr2k,ZSYR2K)();
 FC_GLOBAL(zsyrk,ZSYRK)();
 FC_GLOBAL(ztbmv,ZTBMV)();
 FC_GLOBAL(ztbsv,ZTBSV)();
 FC_GLOBAL(ztpmv,ZTPMV)();
 FC_GLOBAL(ztpsv,ZTPSV)();
 FC_GLOBAL(ztrmm,ZTRMM)();
 FC_GLOBAL(ztrmv,ZTRMV)();
 FC_GLOBAL(ztrsm,ZTRSM)();
 FC_GLOBAL(ztrsv,ZTRSV)();



}



