//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
 */




#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include "flexiblas_config.h"
#include "flexiblas_fortran_mangle.h"

#ifdef FLEXIBLAS_ABI_INTEL
#include "blas_intel.h"
#else
#include "blas_gnu.h"
#endif

HIDDEN void * __flexiblas_blas_addr[1024];

HIDDEN void __flexiblas_dummy_function_to_include_all_blas_symbols(int dummy){
    size_t k = 0;
    void *addr[1024];
    dummy = dummy;
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(caxpy,CAXPY));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ccopy,CCOPY));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cdotc,CDOTC));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cdotu,CDOTU));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cgbmv,CGBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cgemm,CGEMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cgemv,CGEMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cgerc,CGERC));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cgeru,CGERU));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chbmv,CHBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chemm,CHEMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chemv,CHEMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cher2,CHER2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cher2k,CHER2K));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cher,CHER));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cherk,CHERK));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chpmv,CHPMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chpr2,CHPR2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chpr,CHPR));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(crotg,CROTG));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cscal,CSCAL));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(csrot,CSROT));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(csscal,CSSCAL));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cswap,CSWAP));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(csymm,CSYMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(csyr2k,CSYR2K));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(csyrk,CSYRK));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctbmv,CTBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctbsv,CTBSV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctpmv,CTPMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctpsv,CTPSV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctrmm,CTRMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctrmv,CTRMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctrsm,CTRSM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctrsv,CTRSV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dasum,DASUM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(daxpy,DAXPY));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dcopy,DCOPY));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ddot,DDOT));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dgbmv,DGBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dgemm,DGEMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dgemv,DGEMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dger,DGER));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dnrm2,DNRM2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(drot,DROT));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(drotg,DROTG));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(drotm,DROTM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(drotmg,DROTMG));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsbmv,DSBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dscal,DSCAL));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsdot,DSDOT));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dspmv,DSPMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dspr2,DSPR2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dspr,DSPR));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dswap,DSWAP));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsymm,DSYMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsymv,DSYMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsyr2,DSYR2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsyr2k,DSYR2K));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsyr,DSYR));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsyrk,DSYRK));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtbmv,DTBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtbsv,DTBSV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtpmv,DTPMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtpsv,DTPSV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtrmm,DTRMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtrmv,DTRMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtrsm,DTRSM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtrsv,DTRSV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dzasum,DZASUM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dznrm2,DZNRM2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(icamax,ICAMAX));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(idamax,IDAMAX));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(isamax,ISAMAX));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(izamax,IZAMAX));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sasum,SASUM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(saxpy,SAXPY));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(scasum,SCASUM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(scnrm2,SCNRM2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(scopy,SCOPY));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sdot,SDOT));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sdsdot,SDSDOT));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sgbmv,SGBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sgemm,SGEMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sgemv,SGEMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sger,SGER));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(snrm2,SNRM2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(srot,SROT));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(srotg,SROTG));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(srotm,SROTM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(srotmg,SROTMG));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssbmv,SSBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sscal,SSCAL));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sspmv,SSPMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sspr2,SSPR2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sspr,SSPR));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sswap,SSWAP));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssymm,SSYMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssymv,SSYMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssyr2,SSYR2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssyr2k,SSYR2K));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssyr,SSYR));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssyrk,SSYRK));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(stbmv,STBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(stbsv,STBSV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(stpmv,STPMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(stpsv,STPSV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(strmm,STRMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(strmv,STRMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(strsm,STRSM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(strsv,STRSV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zaxpy,ZAXPY));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zcopy,ZCOPY));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zdotc,ZDOTC));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zdotu,ZDOTU));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zdrot,ZDROT));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zdscal,ZDSCAL));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zgbmv,ZGBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zgemm,ZGEMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zgemv,ZGEMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zgerc,ZGERC));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zgeru,ZGERU));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhbmv,ZHBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhemm,ZHEMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhemv,ZHEMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zher2,ZHER2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zher2k,ZHER2K));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zher,ZHER));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zherk,ZHERK));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhpmv,ZHPMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhpr2,ZHPR2));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhpr,ZHPR));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zrotg,ZROTG));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zscal,ZSCAL));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zswap,ZSWAP));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zsymm,ZSYMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zsyr2k,ZSYR2K));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zsyrk,ZSYRK));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztbmv,ZTBMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztbsv,ZTBSV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztpmv,ZTPMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztpsv,ZTPSV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztrmm,ZTRMM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztrmv,ZTRMV));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztrsm,ZTRSM));
    __flexiblas_blas_addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztrsv,ZTRSV));
}



