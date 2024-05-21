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
#include "flexiblas_config.h"
#include "flexiblas_fortran_mangle.h"

#ifdef FLEXIBLAS_ABI_INTEL
#include "blas_intel.h"
#else
#include "blas_gnu.h"
#endif

HIDDEN void __flexiblas_dummy_function_to_include_all_blas_symbols(int dummy){
    size_t k = 0;
    void *addr[1024];
    dummy = dummy;
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(caxpy,CAXPY));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ccopy,CCOPY));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cdotc,CDOTC));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cdotu,CDOTU));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cgbmv,CGBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cgemm,CGEMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cgemv,CGEMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cgerc,CGERC));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cgeru,CGERU));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chbmv,CHBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chemm,CHEMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chemv,CHEMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cher2,CHER2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cher2k,CHER2K));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cher,CHER));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cherk,CHERK));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chpmv,CHPMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chpr2,CHPR2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(chpr,CHPR));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(crotg,CROTG));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cscal,CSCAL));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(csrot,CSROT));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(csscal,CSSCAL));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(cswap,CSWAP));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(csymm,CSYMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(csyr2k,CSYR2K));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(csyrk,CSYRK));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctbmv,CTBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctbsv,CTBSV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctpmv,CTPMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctpsv,CTPSV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctrmm,CTRMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctrmv,CTRMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctrsm,CTRSM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ctrsv,CTRSV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dasum,DASUM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(daxpy,DAXPY));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dcopy,DCOPY));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ddot,DDOT));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dgbmv,DGBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dgemm,DGEMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dgemv,DGEMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dger,DGER));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dnrm2,DNRM2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(drot,DROT));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(drotg,DROTG));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(drotm,DROTM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(drotmg,DROTMG));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsbmv,DSBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dscal,DSCAL));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsdot,DSDOT));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dspmv,DSPMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dspr2,DSPR2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dspr,DSPR));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dswap,DSWAP));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsymm,DSYMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsymv,DSYMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsyr2,DSYR2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsyr2k,DSYR2K));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsyr,DSYR));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dsyrk,DSYRK));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtbmv,DTBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtbsv,DTBSV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtpmv,DTPMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtpsv,DTPSV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtrmm,DTRMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtrmv,DTRMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtrsm,DTRSM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dtrsv,DTRSV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dzasum,DZASUM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(dznrm2,DZNRM2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(icamax,ICAMAX));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(idamax,IDAMAX));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(isamax,ISAMAX));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(izamax,IZAMAX));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sasum,SASUM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(saxpy,SAXPY));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(scasum,SCASUM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(scnrm2,SCNRM2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(scopy,SCOPY));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sdot,SDOT));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sdsdot,SDSDOT));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sgbmv,SGBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sgemm,SGEMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sgemv,SGEMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sger,SGER));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(snrm2,SNRM2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(srot,SROT));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(srotg,SROTG));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(srotm,SROTM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(srotmg,SROTMG));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssbmv,SSBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sscal,SSCAL));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sspmv,SSPMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sspr2,SSPR2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sspr,SSPR));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(sswap,SSWAP));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssymm,SSYMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssymv,SSYMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssyr2,SSYR2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssyr2k,SSYR2K));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssyr,SSYR));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ssyrk,SSYRK));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(stbmv,STBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(stbsv,STBSV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(stpmv,STPMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(stpsv,STPSV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(strmm,STRMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(strmv,STRMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(strsm,STRSM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(strsv,STRSV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zaxpy,ZAXPY));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zcopy,ZCOPY));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zdotc,ZDOTC));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zdotu,ZDOTU));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zdrot,ZDROT));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zdscal,ZDSCAL));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zgbmv,ZGBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zgemm,ZGEMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zgemv,ZGEMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zgerc,ZGERC));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zgeru,ZGERU));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhbmv,ZHBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhemm,ZHEMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhemv,ZHEMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zher2,ZHER2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zher2k,ZHER2K));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zher,ZHER));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zherk,ZHERK));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhpmv,ZHPMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhpr2,ZHPR2));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zhpr,ZHPR));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zrotg,ZROTG));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zscal,ZSCAL));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zswap,ZSWAP));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zsymm,ZSYMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zsyr2k,ZSYR2K));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(zsyrk,ZSYRK));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztbmv,ZTBMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztbsv,ZTBSV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztpmv,ZTPMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztpsv,ZTPSV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztrmm,ZTRMM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztrmv,ZTRMV));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztrsm,ZTRSM));
    addr[k++] = (void*) (uintptr_t) &( FC_GLOBAL(ztrsv,ZTRSV));
    addr[0] = addr[1];
}



