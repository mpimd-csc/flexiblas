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

#ifndef BLAS_STRUCTURE_H
#define BLAS_STRUCTURE_H

#include <stdint.h>
#include <complex.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "flexiblas_config.h"
#include "flexiblas_fortran_char_len.h"
#include "flexiblas_fortran_mangle.h"

typedef struct _flexiblas_blas_backend {
    flexiblas_blasfn caxpby;
    flexiblas_blasfn caxpy;
    flexiblas_blasfn ccopy;
    flexiblas_blasfn cdotc;
    flexiblas_blasfn cdotu;
    flexiblas_blasfn cgbmv;
    flexiblas_blasfn cgeadd;
    flexiblas_blasfn cgemm;
    flexiblas_blasfn cgemmtr;
    flexiblas_blasfn cgemv;
    flexiblas_blasfn cgerc;
    flexiblas_blasfn cgeru;
    flexiblas_blasfn chbmv;
    flexiblas_blasfn chemm;
    flexiblas_blasfn chemv;
    flexiblas_blasfn cher;
    flexiblas_blasfn cher2;
    flexiblas_blasfn cher2k;
    flexiblas_blasfn cherk;
    flexiblas_blasfn chpmv;
    flexiblas_blasfn chpr;
    flexiblas_blasfn chpr2;
    flexiblas_blasfn cimatcopy;
    flexiblas_blasfn comatcopy;
    flexiblas_blasfn crotg;
    flexiblas_blasfn cscal;
    flexiblas_blasfn csrot;
    flexiblas_blasfn csscal;
    flexiblas_blasfn cswap;
    flexiblas_blasfn csymm;
    flexiblas_blasfn csyr2k;
    flexiblas_blasfn csyrk;
    flexiblas_blasfn ctbmv;
    flexiblas_blasfn ctbsv;
    flexiblas_blasfn ctpmv;
    flexiblas_blasfn ctpsv;
    flexiblas_blasfn ctrmm;
    flexiblas_blasfn ctrmv;
    flexiblas_blasfn ctrsm;
    flexiblas_blasfn ctrsv;
    flexiblas_blasfn dasum;
    flexiblas_blasfn daxpby;
    flexiblas_blasfn daxpy;
    flexiblas_blasfn dcopy;
    flexiblas_blasfn ddot;
    flexiblas_blasfn dgbmv;
    flexiblas_blasfn dgeadd;
    flexiblas_blasfn dgemm;
    flexiblas_blasfn dgemmtr;
    flexiblas_blasfn dgemv;
    flexiblas_blasfn dger;
    flexiblas_blasfn dimatcopy;
    flexiblas_blasfn dnrm2;
    flexiblas_blasfn domatcopy;
    flexiblas_blasfn drot;
    flexiblas_blasfn drotg;
    flexiblas_blasfn drotm;
    flexiblas_blasfn drotmg;
    flexiblas_blasfn dsbmv;
    flexiblas_blasfn dscal;
    flexiblas_blasfn dsdot;
    flexiblas_blasfn dspmv;
    flexiblas_blasfn dspr;
    flexiblas_blasfn dspr2;
    flexiblas_blasfn dswap;
    flexiblas_blasfn dsymm;
    flexiblas_blasfn dsymv;
    flexiblas_blasfn dsyr;
    flexiblas_blasfn dsyr2;
    flexiblas_blasfn dsyr2k;
    flexiblas_blasfn dsyrk;
    flexiblas_blasfn dtbmv;
    flexiblas_blasfn dtbsv;
    flexiblas_blasfn dtpmv;
    flexiblas_blasfn dtpsv;
    flexiblas_blasfn dtrmm;
    flexiblas_blasfn dtrmv;
    flexiblas_blasfn dtrsm;
    flexiblas_blasfn dtrsv;
    flexiblas_blasfn dzasum;
    flexiblas_blasfn dznrm2;
    flexiblas_blasfn icamax;
    flexiblas_blasfn idamax;
    flexiblas_blasfn isamax;
    flexiblas_blasfn izamax;
    flexiblas_blasfn sasum;
    flexiblas_blasfn saxpby;
    flexiblas_blasfn saxpy;
    flexiblas_blasfn scasum;
    flexiblas_blasfn scnrm2;
    flexiblas_blasfn scopy;
    flexiblas_blasfn sdot;
    flexiblas_blasfn sdsdot;
    flexiblas_blasfn sgbmv;
    flexiblas_blasfn sgeadd;
    flexiblas_blasfn sgemm;
    flexiblas_blasfn sgemmtr;
    flexiblas_blasfn sgemv;
    flexiblas_blasfn sger;
    flexiblas_blasfn simatcopy;
    flexiblas_blasfn snrm2;
    flexiblas_blasfn somatcopy;
    flexiblas_blasfn srot;
    flexiblas_blasfn srotg;
    flexiblas_blasfn srotm;
    flexiblas_blasfn srotmg;
    flexiblas_blasfn ssbmv;
    flexiblas_blasfn sscal;
    flexiblas_blasfn sspmv;
    flexiblas_blasfn sspr;
    flexiblas_blasfn sspr2;
    flexiblas_blasfn sswap;
    flexiblas_blasfn ssymm;
    flexiblas_blasfn ssymv;
    flexiblas_blasfn ssyr;
    flexiblas_blasfn ssyr2;
    flexiblas_blasfn ssyr2k;
    flexiblas_blasfn ssyrk;
    flexiblas_blasfn stbmv;
    flexiblas_blasfn stbsv;
    flexiblas_blasfn stpmv;
    flexiblas_blasfn stpsv;
    flexiblas_blasfn strmm;
    flexiblas_blasfn strmv;
    flexiblas_blasfn strsm;
    flexiblas_blasfn strsv;
    flexiblas_blasfn zaxpby;
    flexiblas_blasfn zaxpy;
    flexiblas_blasfn zcopy;
    flexiblas_blasfn zdotc;
    flexiblas_blasfn zdotu;
    flexiblas_blasfn zdrot;
    flexiblas_blasfn zdscal;
    flexiblas_blasfn zgbmv;
    flexiblas_blasfn zgeadd;
    flexiblas_blasfn zgemm;
    flexiblas_blasfn zgemmtr;
    flexiblas_blasfn zgemv;
    flexiblas_blasfn zgerc;
    flexiblas_blasfn zgeru;
    flexiblas_blasfn zhbmv;
    flexiblas_blasfn zhemm;
    flexiblas_blasfn zhemv;
    flexiblas_blasfn zher;
    flexiblas_blasfn zher2;
    flexiblas_blasfn zher2k;
    flexiblas_blasfn zherk;
    flexiblas_blasfn zhpmv;
    flexiblas_blasfn zhpr;
    flexiblas_blasfn zhpr2;
    flexiblas_blasfn zimatcopy;
    flexiblas_blasfn zomatcopy;
    flexiblas_blasfn zrotg;
    flexiblas_blasfn zscal;
    flexiblas_blasfn zswap;
    flexiblas_blasfn zsymm;
    flexiblas_blasfn zsyr2k;
    flexiblas_blasfn zsyrk;
    flexiblas_blasfn ztbmv;
    flexiblas_blasfn ztbsv;
    flexiblas_blasfn ztpmv;
    flexiblas_blasfn ztpsv;
    flexiblas_blasfn ztrmm;
    flexiblas_blasfn ztrmv;
    flexiblas_blasfn ztrsm;
    flexiblas_blasfn ztrsv;

} flexiblas_blas_backend_t;

#ifdef __cplusplus
}
#endif

#endif

