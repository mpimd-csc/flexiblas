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

#ifndef CBLAS_STRUCTURE_H
#define CBLAS_STRUCTURE_H

#include <stdint.h>
#include <complex.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "flexiblas_config.h"
#include "flexiblas_fortran_char_len.h"
#include "flexiblas_fortran_mangle.h"

typedef struct _flexiblas_cblas_backend {
    flexiblas_blasfn cblas_caxpby;
    flexiblas_blasfn cblas_caxpy;
    flexiblas_blasfn cblas_ccopy;
    flexiblas_blasfn cblas_cdotc_sub;
    flexiblas_blasfn cblas_cdotu_sub;
    flexiblas_blasfn cblas_cgbmv;
    flexiblas_blasfn cblas_cgeadd;
    flexiblas_blasfn cblas_cgemm;
    flexiblas_blasfn cblas_cgemmtr;
    flexiblas_blasfn cblas_cgemv;
    flexiblas_blasfn cblas_cgerc;
    flexiblas_blasfn cblas_cgeru;
    flexiblas_blasfn cblas_chbmv;
    flexiblas_blasfn cblas_chemm;
    flexiblas_blasfn cblas_chemv;
    flexiblas_blasfn cblas_cher;
    flexiblas_blasfn cblas_cher2;
    flexiblas_blasfn cblas_cher2k;
    flexiblas_blasfn cblas_cherk;
    flexiblas_blasfn cblas_chpmv;
    flexiblas_blasfn cblas_chpr;
    flexiblas_blasfn cblas_chpr2;
    flexiblas_blasfn cblas_cimatcopy;
    flexiblas_blasfn cblas_comatcopy;
    flexiblas_blasfn cblas_crotg;
    flexiblas_blasfn cblas_cscal;
    flexiblas_blasfn cblas_csrot;
    flexiblas_blasfn cblas_csscal;
    flexiblas_blasfn cblas_cswap;
    flexiblas_blasfn cblas_csymm;
    flexiblas_blasfn cblas_csyr2k;
    flexiblas_blasfn cblas_csyrk;
    flexiblas_blasfn cblas_ctbmv;
    flexiblas_blasfn cblas_ctbsv;
    flexiblas_blasfn cblas_ctpmv;
    flexiblas_blasfn cblas_ctpsv;
    flexiblas_blasfn cblas_ctrmm;
    flexiblas_blasfn cblas_ctrmv;
    flexiblas_blasfn cblas_ctrsm;
    flexiblas_blasfn cblas_ctrsv;
    flexiblas_blasfn cblas_dasum;
    flexiblas_blasfn cblas_daxpby;
    flexiblas_blasfn cblas_daxpy;
    flexiblas_blasfn cblas_dcopy;
    flexiblas_blasfn cblas_ddot;
    flexiblas_blasfn cblas_dgbmv;
    flexiblas_blasfn cblas_dgeadd;
    flexiblas_blasfn cblas_dgemm;
    flexiblas_blasfn cblas_dgemmtr;
    flexiblas_blasfn cblas_dgemv;
    flexiblas_blasfn cblas_dger;
    flexiblas_blasfn cblas_dimatcopy;
    flexiblas_blasfn cblas_dnrm2;
    flexiblas_blasfn cblas_domatcopy;
    flexiblas_blasfn cblas_drot;
    flexiblas_blasfn cblas_drotg;
    flexiblas_blasfn cblas_drotm;
    flexiblas_blasfn cblas_drotmg;
    flexiblas_blasfn cblas_dsbmv;
    flexiblas_blasfn cblas_dscal;
    flexiblas_blasfn cblas_dsdot;
    flexiblas_blasfn cblas_dspmv;
    flexiblas_blasfn cblas_dspr;
    flexiblas_blasfn cblas_dspr2;
    flexiblas_blasfn cblas_dswap;
    flexiblas_blasfn cblas_dsymm;
    flexiblas_blasfn cblas_dsymv;
    flexiblas_blasfn cblas_dsyr;
    flexiblas_blasfn cblas_dsyr2;
    flexiblas_blasfn cblas_dsyr2k;
    flexiblas_blasfn cblas_dsyrk;
    flexiblas_blasfn cblas_dtbmv;
    flexiblas_blasfn cblas_dtbsv;
    flexiblas_blasfn cblas_dtpmv;
    flexiblas_blasfn cblas_dtpsv;
    flexiblas_blasfn cblas_dtrmm;
    flexiblas_blasfn cblas_dtrmv;
    flexiblas_blasfn cblas_dtrsm;
    flexiblas_blasfn cblas_dtrsv;
    flexiblas_blasfn cblas_dzasum;
    flexiblas_blasfn cblas_dznrm2;
    flexiblas_blasfn cblas_icamax;
    flexiblas_blasfn cblas_idamax;
    flexiblas_blasfn cblas_isamax;
    flexiblas_blasfn cblas_izamax;
    flexiblas_blasfn cblas_sasum;
    flexiblas_blasfn cblas_saxpby;
    flexiblas_blasfn cblas_saxpy;
    flexiblas_blasfn cblas_scasum;
    flexiblas_blasfn cblas_scnrm2;
    flexiblas_blasfn cblas_scopy;
    flexiblas_blasfn cblas_sdot;
    flexiblas_blasfn cblas_sdsdot;
    flexiblas_blasfn cblas_sgbmv;
    flexiblas_blasfn cblas_sgeadd;
    flexiblas_blasfn cblas_sgemm;
    flexiblas_blasfn cblas_sgemmtr;
    flexiblas_blasfn cblas_sgemv;
    flexiblas_blasfn cblas_sger;
    flexiblas_blasfn cblas_simatcopy;
    flexiblas_blasfn cblas_snrm2;
    flexiblas_blasfn cblas_somatcopy;
    flexiblas_blasfn cblas_srot;
    flexiblas_blasfn cblas_srotg;
    flexiblas_blasfn cblas_srotm;
    flexiblas_blasfn cblas_srotmg;
    flexiblas_blasfn cblas_ssbmv;
    flexiblas_blasfn cblas_sscal;
    flexiblas_blasfn cblas_sspmv;
    flexiblas_blasfn cblas_sspr;
    flexiblas_blasfn cblas_sspr2;
    flexiblas_blasfn cblas_sswap;
    flexiblas_blasfn cblas_ssymm;
    flexiblas_blasfn cblas_ssymv;
    flexiblas_blasfn cblas_ssyr;
    flexiblas_blasfn cblas_ssyr2;
    flexiblas_blasfn cblas_ssyr2k;
    flexiblas_blasfn cblas_ssyrk;
    flexiblas_blasfn cblas_stbmv;
    flexiblas_blasfn cblas_stbsv;
    flexiblas_blasfn cblas_stpmv;
    flexiblas_blasfn cblas_stpsv;
    flexiblas_blasfn cblas_strmm;
    flexiblas_blasfn cblas_strmv;
    flexiblas_blasfn cblas_strsm;
    flexiblas_blasfn cblas_strsv;
    flexiblas_blasfn cblas_zaxpby;
    flexiblas_blasfn cblas_zaxpy;
    flexiblas_blasfn cblas_zcopy;
    flexiblas_blasfn cblas_zdotc_sub;
    flexiblas_blasfn cblas_zdotu_sub;
    flexiblas_blasfn cblas_zdrot;
    flexiblas_blasfn cblas_zdscal;
    flexiblas_blasfn cblas_zgbmv;
    flexiblas_blasfn cblas_zgeadd;
    flexiblas_blasfn cblas_zgemm;
    flexiblas_blasfn cblas_zgemmtr;
    flexiblas_blasfn cblas_zgemv;
    flexiblas_blasfn cblas_zgerc;
    flexiblas_blasfn cblas_zgeru;
    flexiblas_blasfn cblas_zhbmv;
    flexiblas_blasfn cblas_zhemm;
    flexiblas_blasfn cblas_zhemv;
    flexiblas_blasfn cblas_zher;
    flexiblas_blasfn cblas_zher2;
    flexiblas_blasfn cblas_zher2k;
    flexiblas_blasfn cblas_zherk;
    flexiblas_blasfn cblas_zhpmv;
    flexiblas_blasfn cblas_zhpr;
    flexiblas_blasfn cblas_zhpr2;
    flexiblas_blasfn cblas_zimatcopy;
    flexiblas_blasfn cblas_zomatcopy;
    flexiblas_blasfn cblas_zrotg;
    flexiblas_blasfn cblas_zscal;
    flexiblas_blasfn cblas_zswap;
    flexiblas_blasfn cblas_zsymm;
    flexiblas_blasfn cblas_zsyr2k;
    flexiblas_blasfn cblas_zsyrk;
    flexiblas_blasfn cblas_ztbmv;
    flexiblas_blasfn cblas_ztbsv;
    flexiblas_blasfn cblas_ztpmv;
    flexiblas_blasfn cblas_ztpsv;
    flexiblas_blasfn cblas_ztrmm;
    flexiblas_blasfn cblas_ztrmv;
    flexiblas_blasfn cblas_ztrsm;
    flexiblas_blasfn cblas_ztrsv;

} flexiblas_cblas_backend_t;

#ifdef __cplusplus
}
#endif

#endif

