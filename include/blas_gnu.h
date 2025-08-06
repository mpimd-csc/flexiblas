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

#ifndef BLAS_H
#define BLAS_H

#include <stdint.h>

/* Complex type (single precision) */

#ifndef lapack_complex_float
#ifndef __cplusplus
#include <complex.h>
#else
#include <complex>
#endif
#define lapack_complex_float    float _Complex
#endif

#ifndef blas_complex_float
#ifndef __cplusplus
#include <complex.h>
#else
#include <complex>
#endif
#define blas_complex_float    float _Complex
#endif


/* Complex type (double precision) */
#ifndef lapack_complex_double
#ifndef __cplusplus
#include <complex.h>
#else
#include <complex>
#endif
#define lapack_complex_double   double _Complex
#endif

#ifndef blas_complex_double
#ifndef __cplusplus
#include <complex.h>
#else
#include <complex>
#endif
#define blas_complex_double   double _Complex
#endif


#ifdef __cplusplus
extern "C" {
#endif

#include "flexiblas_config.h"
#include "flexiblas_fortran_char_len.h"
#include "flexiblas_fortran_mangle.h"

    void FC_GLOBAL(caxpby,CAXPBY)(blasint *n, blas_complex_float *ca, blas_complex_float *cx, blasint *incx, blas_complex_float *cb, blas_complex_float *cy, blasint *incy);

    void FC_GLOBAL(caxpy,CAXPY)(blasint *n, blas_complex_float *ca, blas_complex_float *cx, blasint *incx, blas_complex_float *cy, blasint *incy);

    void FC_GLOBAL(ccopy,CCOPY)(blasint *n, blas_complex_float *cx, blasint *incx, blas_complex_float *cy, blasint *incy);

    blas_complex_float FC_GLOBAL(cdotc,CDOTC)(blasint *n, blas_complex_float *cx, blasint *incx, blas_complex_float *cy, blasint *incy);

    blas_complex_float FC_GLOBAL(cdotu,CDOTU)(blasint *n, blas_complex_float *cx, blasint *incx, blas_complex_float *cy, blasint *incy);

    void FC_GLOBAL(cgbmv,CGBMV)(char *trans, blasint *m, blasint *n, blasint *kl, blasint *ku, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *x, blasint *incx, blas_complex_float *beta, blas_complex_float *y, blasint *incy, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(cgeadd,CGEADD)(blasint *m, blasint *n, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *beta, blas_complex_float *b, blasint *ldb);

    void FC_GLOBAL(cgemm,CGEMM)(char *transa, char *transb, blasint *m, blasint *n, blasint *k, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *b, blasint *ldb, blas_complex_float *beta, blas_complex_float *c, blasint *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(cgemmtr,CGEMMTR)(char *uplo, char *transa, char *transb, blasint *n, blasint *k, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *b, blasint *ldb, blas_complex_float *beta, blas_complex_float *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(cgemmt,CGEMMT)(char *uplo, char *transa, char *transb, blasint *n, blasint *k, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *b, blasint *ldb, blas_complex_float *beta, blas_complex_float *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(cgemv,CGEMV)(char *trans, blasint *m, blasint *n, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *x, blasint *incx, blas_complex_float *beta, blas_complex_float *y, blasint *incy, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(cgerc,CGERC)(blasint *m, blasint *n, blas_complex_float *alpha, blas_complex_float *x, blasint *incx, blas_complex_float *y, blasint *incy, blas_complex_float *a, blasint *lda);

    void FC_GLOBAL(cgeru,CGERU)(blasint *m, blasint *n, blas_complex_float *alpha, blas_complex_float *x, blasint *incx, blas_complex_float *y, blasint *incy, blas_complex_float *a, blasint *lda);

    void FC_GLOBAL(chbmv,CHBMV)(char *uplo, blasint *n, blasint *k, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *x, blasint *incx, blas_complex_float *beta, blas_complex_float *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(chemm,CHEMM)(char *side, char *uplo, blasint *m, blasint *n, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *b, blasint *ldb, blas_complex_float *beta, blas_complex_float *c, blasint *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(chemv,CHEMV)(char *uplo, blasint *n, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *x, blasint *incx, blas_complex_float *beta, blas_complex_float *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(cher,CHER)(char *uplo, blasint *n, float *alpha, blas_complex_float *x, blasint *incx, blas_complex_float *a, blasint *lda, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(cher2,CHER2)(char *uplo, blasint *n, blas_complex_float *alpha, blas_complex_float *x, blasint *incx, blas_complex_float *y, blasint *incy, blas_complex_float *a, blasint *lda, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(cher2k,CHER2K)(char *uplo, char *trans, blasint *n, blasint *k, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *b, blasint *ldb, float *beta, blas_complex_float *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(cherk,CHERK)(char *uplo, char *trans, blasint *n, blasint *k, float *alpha, blas_complex_float *a, blasint *lda, float *beta, blas_complex_float *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(chpmv,CHPMV)(char *uplo, blasint *n, blas_complex_float *alpha, blas_complex_float *ap, blas_complex_float *x, blasint *incx, blas_complex_float *beta, blas_complex_float *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(chpr,CHPR)(char *uplo, blasint *n, float *alpha, blas_complex_float *x, blasint *incx, blas_complex_float *ap, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(chpr2,CHPR2)(char *uplo, blasint *n, blas_complex_float *alpha, blas_complex_float *x, blasint *incx, blas_complex_float *y, blasint *incy, blas_complex_float *ap, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(cimatcopy,CIMATCOPY)(char *ORDER, char *TRANS, blasint *rows, blasint *cols, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blasint *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(comatcopy,COMATCOPY)(char *ORDER, char *TRANS, blasint *rows, blasint *cols, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *b, blasint *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);

    void FC_GLOBAL(crotg,CROTG)(blas_complex_float *a, blas_complex_float *b, float *c, blas_complex_float *s);

    void FC_GLOBAL(cscal,CSCAL)(blasint *n, blas_complex_float *ca, blas_complex_float *cx, blasint *incx);

    void FC_GLOBAL(csrot,CSROT)(blasint *n, blas_complex_float *cx, blasint *incx, blas_complex_float *cy, blasint *incy, float *c, float *s);

    void FC_GLOBAL(csscal,CSSCAL)(blasint *n, float *sa, blas_complex_float *cx, blasint *incx);

    void FC_GLOBAL(cswap,CSWAP)(blasint *n, blas_complex_float *cx, blasint *incx, blas_complex_float *cy, blasint *incy);

    void FC_GLOBAL(csymm,CSYMM)(char *side, char *uplo, blasint *m, blasint *n, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *b, blasint *ldb, blas_complex_float *beta, blas_complex_float *c, blasint *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(csyr2k,CSYR2K)(char *uplo, char *trans, blasint *n, blasint *k, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *b, blasint *ldb, blas_complex_float *beta, blas_complex_float *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(csyrk,CSYRK)(char *uplo, char *trans, blasint *n, blasint *k, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *beta, blas_complex_float *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(ctbmv,CTBMV)(char *uplo, char *trans, char *diag, blasint *n, blasint *k, blas_complex_float *a, blasint *lda, blas_complex_float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ctbsv,CTBSV)(char *uplo, char *trans, char *diag, blasint *n, blasint *k, blas_complex_float *a, blasint *lda, blas_complex_float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ctpmv,CTPMV)(char *uplo, char *trans, char *diag, blasint *n, blas_complex_float *ap, blas_complex_float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ctpsv,CTPSV)(char *uplo, char *trans, char *diag, blasint *n, blas_complex_float *ap, blas_complex_float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ctrmm,CTRMM)(char *side, char *uplo, char *transa, char *diag, blasint *m, blasint *n, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *b, blasint *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ctrmv,CTRMV)(char *uplo, char *trans, char *diag, blasint *n, blas_complex_float *a, blasint *lda, blas_complex_float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ctrsm,CTRSM)(char *side, char *uplo, char *transa, char *diag, blasint *m, blasint *n, blas_complex_float *alpha, blas_complex_float *a, blasint *lda, blas_complex_float *b, blasint *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ctrsv,CTRSV)(char *uplo, char *trans, char *diag, blasint *n, blas_complex_float *a, blasint *lda, blas_complex_float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    double FC_GLOBAL(dasum,DASUM)(blasint *n, double *dx, blasint *incx);

    void FC_GLOBAL(daxpby,DAXPBY)(blasint *n, double *da, double *dx, blasint *incx, double *db, double *dy, blasint *incy);

    void FC_GLOBAL(daxpy,DAXPY)(blasint *n, double *da, double *dx, blasint *incx, double *dy, blasint *incy);

    void FC_GLOBAL(dcopy,DCOPY)(blasint *n, double *dx, blasint *incx, double *dy, blasint *incy);

    double FC_GLOBAL(ddot,DDOT)(blasint *n, double *dx, blasint *incx, double *dy, blasint *incy);

    void FC_GLOBAL(dgbmv,DGBMV)(char *trans, blasint *m, blasint *n, blasint *kl, blasint *ku, double *alpha, double *a, blasint *lda, double *x, blasint *incx, double *beta, double *y, blasint *incy, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(dgeadd,DGEADD)(blasint *m, blasint *n, double *alpha, double *a, blasint *lda, double *beta, double *b, blasint *ldb);

    void FC_GLOBAL(dgemm,DGEMM)(char *transa, char *transb, blasint *m, blasint *n, blasint *k, double *alpha, double *a, blasint *lda, double *b, blasint *ldb, double *beta, double *c, blasint *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(dgemmtr,DGEMMTR)(char *uplo, char *transa, char *transb, blasint *n, blasint *k, double *alpha, double *a, blasint *lda, double *b, blasint *ldb, double *beta, double *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(dgemmt,DGEMMT)(char *uplo, char *transa, char *transb, blasint *n, blasint *k, double *alpha, double *a, blasint *lda, double *b, blasint *ldb, double *beta, double *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(dgemv,DGEMV)(char *trans, blasint *m, blasint *n, double *alpha, double *a, blasint *lda, double *x, blasint *incx, double *beta, double *y, blasint *incy, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(dger,DGER)(blasint *m, blasint *n, double *alpha, double *x, blasint *incx, double *y, blasint *incy, double *a, blasint *lda);

    void FC_GLOBAL(dimatcopy,DIMATCOPY)(char *ORDER, char *TRANS, blasint *rows, blasint *cols, double *alpha, double *a, blasint *lda, blasint *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);

    double FC_GLOBAL(dnrm2,DNRM2)(blasint *n, double *x, blasint *incx);

    void FC_GLOBAL(domatcopy,DOMATCOPY)(char *ORDER, char *TRANS, blasint *rows, blasint *cols, double *alpha, double *a, blasint *lda, double *b, blasint *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);

    void FC_GLOBAL(drot,DROT)(blasint *n, double *dx, blasint *incx, double *dy, blasint *incy, double *c, double *s);

    void FC_GLOBAL(drotg,DROTG)(double *a, double *b, double *c, double *s);

    void FC_GLOBAL(drotm,DROTM)(blasint *n, double *dx, blasint *incx, double *dy, blasint *incy, double *dparam);

    void FC_GLOBAL(drotmg,DROTMG)(double *dd1, double *dd2, double *dx1, double *dy1, double *dparam);

    void FC_GLOBAL(dsbmv,DSBMV)(char *uplo, blasint *n, blasint *k, double *alpha, double *a, blasint *lda, double *x, blasint *incx, double *beta, double *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(dscal,DSCAL)(blasint *n, double *da, double *dx, blasint *incx);

    double FC_GLOBAL(dsdot,DSDOT)(blasint *n, float *sx, blasint *incx, float *sy, blasint *incy);

    void FC_GLOBAL(dspmv,DSPMV)(char *uplo, blasint *n, double *alpha, double *ap, double *x, blasint *incx, double *beta, double *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(dspr,DSPR)(char *uplo, blasint *n, double *alpha, double *x, blasint *incx, double *ap, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(dspr2,DSPR2)(char *uplo, blasint *n, double *alpha, double *x, blasint *incx, double *y, blasint *incy, double *ap, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(dswap,DSWAP)(blasint *n, double *dx, blasint *incx, double *dy, blasint *incy);

    void FC_GLOBAL(dsymm,DSYMM)(char *side, char *uplo, blasint *m, blasint *n, double *alpha, double *a, blasint *lda, double *b, blasint *ldb, double *beta, double *c, blasint *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(dsymv,DSYMV)(char *uplo, blasint *n, double *alpha, double *a, blasint *lda, double *x, blasint *incx, double *beta, double *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(dsyr,DSYR)(char *uplo, blasint *n, double *alpha, double *x, blasint *incx, double *a, blasint *lda, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(dsyr2,DSYR2)(char *uplo, blasint *n, double *alpha, double *x, blasint *incx, double *y, blasint *incy, double *a, blasint *lda, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(dsyr2k,DSYR2K)(char *uplo, char *trans, blasint *n, blasint *k, double *alpha, double *a, blasint *lda, double *b, blasint *ldb, double *beta, double *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(dsyrk,DSYRK)(char *uplo, char *trans, blasint *n, blasint *k, double *alpha, double *a, blasint *lda, double *beta, double *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(dtbmv,DTBMV)(char *uplo, char *trans, char *diag, blasint *n, blasint *k, double *a, blasint *lda, double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(dtbsv,DTBSV)(char *uplo, char *trans, char *diag, blasint *n, blasint *k, double *a, blasint *lda, double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(dtpmv,DTPMV)(char *uplo, char *trans, char *diag, blasint *n, double *ap, double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(dtpsv,DTPSV)(char *uplo, char *trans, char *diag, blasint *n, double *ap, double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(dtrmm,DTRMM)(char *side, char *uplo, char *transa, char *diag, blasint *m, blasint *n, double *alpha, double *a, blasint *lda, double *b, blasint *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(dtrmv,DTRMV)(char *uplo, char *trans, char *diag, blasint *n, double *a, blasint *lda, double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(dtrsm,DTRSM)(char *side, char *uplo, char *transa, char *diag, blasint *m, blasint *n, double *alpha, double *a, blasint *lda, double *b, blasint *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(dtrsv,DTRSV)(char *uplo, char *trans, char *diag, blasint *n, double *a, blasint *lda, double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    double FC_GLOBAL(dzasum,DZASUM)(blasint *n, blas_complex_double *zx, blasint *incx);

    double FC_GLOBAL(dznrm2,DZNRM2)(blasint *n, blas_complex_double *x, blasint *incx);

    blasint FC_GLOBAL(icamax,ICAMAX)(blasint *n, blas_complex_float *cx, blasint *incx);

    blasint FC_GLOBAL(idamax,IDAMAX)(blasint *n, double *dx, blasint *incx);

    blasint FC_GLOBAL(isamax,ISAMAX)(blasint *n, float *sx, blasint *incx);

    blasint FC_GLOBAL(izamax,IZAMAX)(blasint *n, blas_complex_double *zx, blasint *incx);

    float FC_GLOBAL(sasum,SASUM)(blasint *n, float *sx, blasint *incx);

    void FC_GLOBAL(saxpby,SAXPBY)(blasint *n, float *sa, float *sx, blasint *incx, float *sb, float *sy, blasint *incy);

    void FC_GLOBAL(saxpy,SAXPY)(blasint *n, float *sa, float *sx, blasint *incx, float *sy, blasint *incy);

    float FC_GLOBAL(scasum,SCASUM)(blasint *n, blas_complex_float *cx, blasint *incx);

    float FC_GLOBAL(scnrm2,SCNRM2)(blasint *n, blas_complex_float *x, blasint *incx);

    void FC_GLOBAL(scopy,SCOPY)(blasint *n, float *sx, blasint *incx, float *sy, blasint *incy);

    float FC_GLOBAL(sdot,SDOT)(blasint *n, float *sx, blasint *incx, float *sy, blasint *incy);

    float FC_GLOBAL(sdsdot,SDSDOT)(blasint *n, float *sb, float *sx, blasint *incx, float *sy, blasint *incy);

    void FC_GLOBAL(sgbmv,SGBMV)(char *trans, blasint *m, blasint *n, blasint *kl, blasint *ku, float *alpha, float *a, blasint *lda, float *x, blasint *incx, float *beta, float *y, blasint *incy, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(sgeadd,SGEADD)(blasint *m, blasint *n, float *alpha, float *a, blasint *lda, float *beta, float *b, blasint *ldb);

    void FC_GLOBAL(sgemm,SGEMM)(char *transa, char *transb, blasint *m, blasint *n, blasint *k, float *alpha, float *a, blasint *lda, float *b, blasint *ldb, float *beta, float *c, blasint *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(sgemmtr,SGEMMTR)(char *uplo, char *transa, char *transb, blasint *n, blasint *k, float *alpha, float *a, blasint *lda, float *b, blasint *ldb, float *beta, float *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(sgemmt,SGEMMT)(char *uplo, char *transa, char *transb, blasint *n, blasint *k, float *alpha, float *a, blasint *lda, float *b, blasint *ldb, float *beta, float *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(sgemv,SGEMV)(char *trans, blasint *m, blasint *n, float *alpha, float *a, blasint *lda, float *x, blasint *incx, float *beta, float *y, blasint *incy, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(sger,SGER)(blasint *m, blasint *n, float *alpha, float *x, blasint *incx, float *y, blasint *incy, float *a, blasint *lda);

    void FC_GLOBAL(simatcopy,SIMATCOPY)(char *ORDER, char *TRANS, blasint *rows, blasint *cols, float *alpha, float *a, blasint *lda, blasint *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);

    float FC_GLOBAL(snrm2,SNRM2)(blasint *n, float *x, blasint *incx);

    void FC_GLOBAL(somatcopy,SOMATCOPY)(char *ORDER, char *TRANS, blasint *rows, blasint *cols, float *alpha, float *a, blasint *lda, float *b, blasint *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);

    void FC_GLOBAL(srot,SROT)(blasint *n, float *sx, blasint *incx, float *sy, blasint *incy, float *c, float *s);

    void FC_GLOBAL(srotg,SROTG)(float *a, float *b, float *c, float *s);

    void FC_GLOBAL(srotm,SROTM)(blasint *n, float *sx, blasint *incx, float *sy, blasint *incy, float *sparam);

    void FC_GLOBAL(srotmg,SROTMG)(float *sd1, float *sd2, float *sx1, float *sy1, float *sparam);

    void FC_GLOBAL(ssbmv,SSBMV)(char *uplo, blasint *n, blasint *k, float *alpha, float *a, blasint *lda, float *x, blasint *incx, float *beta, float *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(sscal,SSCAL)(blasint *n, float *sa, float *sx, blasint *incx);

    void FC_GLOBAL(sspmv,SSPMV)(char *uplo, blasint *n, float *alpha, float *ap, float *x, blasint *incx, float *beta, float *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(sspr,SSPR)(char *uplo, blasint *n, float *alpha, float *x, blasint *incx, float *ap, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(sspr2,SSPR2)(char *uplo, blasint *n, float *alpha, float *x, blasint *incx, float *y, blasint *incy, float *ap, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(sswap,SSWAP)(blasint *n, float *sx, blasint *incx, float *sy, blasint *incy);

    void FC_GLOBAL(ssymm,SSYMM)(char *side, char *uplo, blasint *m, blasint *n, float *alpha, float *a, blasint *lda, float *b, blasint *ldb, float *beta, float *c, blasint *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(ssymv,SSYMV)(char *uplo, blasint *n, float *alpha, float *a, blasint *lda, float *x, blasint *incx, float *beta, float *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(ssyr,SSYR)(char *uplo, blasint *n, float *alpha, float *x, blasint *incx, float *a, blasint *lda, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(ssyr2,SSYR2)(char *uplo, blasint *n, float *alpha, float *x, blasint *incx, float *y, blasint *incy, float *a, blasint *lda, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(ssyr2k,SSYR2K)(char *uplo, char *trans, blasint *n, blasint *k, float *alpha, float *a, blasint *lda, float *b, blasint *ldb, float *beta, float *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(ssyrk,SSYRK)(char *uplo, char *trans, blasint *n, blasint *k, float *alpha, float *a, blasint *lda, float *beta, float *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(stbmv,STBMV)(char *uplo, char *trans, char *diag, blasint *n, blasint *k, float *a, blasint *lda, float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(stbsv,STBSV)(char *uplo, char *trans, char *diag, blasint *n, blasint *k, float *a, blasint *lda, float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(stpmv,STPMV)(char *uplo, char *trans, char *diag, blasint *n, float *ap, float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(stpsv,STPSV)(char *uplo, char *trans, char *diag, blasint *n, float *ap, float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(strmm,STRMM)(char *side, char *uplo, char *transa, char *diag, blasint *m, blasint *n, float *alpha, float *a, blasint *lda, float *b, blasint *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(strmv,STRMV)(char *uplo, char *trans, char *diag, blasint *n, float *a, blasint *lda, float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(strsm,STRSM)(char *side, char *uplo, char *transa, char *diag, blasint *m, blasint *n, float *alpha, float *a, blasint *lda, float *b, blasint *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(strsv,STRSV)(char *uplo, char *trans, char *diag, blasint *n, float *a, blasint *lda, float *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(zaxpby,ZAXPBY)(blasint *n, blas_complex_double *za, blas_complex_double *zx, blasint *incx, blas_complex_double *zb, blas_complex_double *zy, blasint *incy);

    void FC_GLOBAL(zaxpy,ZAXPY)(blasint *n, blas_complex_double *za, blas_complex_double *zx, blasint *incx, blas_complex_double *zy, blasint *incy);

    void FC_GLOBAL(zcopy,ZCOPY)(blasint *n, blas_complex_double *zx, blasint *incx, blas_complex_double *zy, blasint *incy);

    blas_complex_double FC_GLOBAL(zdotc,ZDOTC)(blasint *n, blas_complex_double *zx, blasint *incx, blas_complex_double *zy, blasint *incy);

    blas_complex_double FC_GLOBAL(zdotu,ZDOTU)(blasint *n, blas_complex_double *zx, blasint *incx, blas_complex_double *zy, blasint *incy);

    void FC_GLOBAL(zdrot,ZDROT)(blasint *n, blas_complex_double *zx, blasint *incx, blas_complex_double *zy, blasint *incy, double *c, double *s);

    void FC_GLOBAL(zdscal,ZDSCAL)(blasint *n, double *da, blas_complex_double *zx, blasint *incx);

    void FC_GLOBAL(zgbmv,ZGBMV)(char *trans, blasint *m, blasint *n, blasint *kl, blasint *ku, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *x, blasint *incx, blas_complex_double *beta, blas_complex_double *y, blasint *incy, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(zgeadd,ZGEADD)(blasint *m, blasint *n, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *beta, blas_complex_double *b, blasint *ldb);

    void FC_GLOBAL(zgemm,ZGEMM)(char *transa, char *transb, blasint *m, blasint *n, blasint *k, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *b, blasint *ldb, blas_complex_double *beta, blas_complex_double *c, blasint *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(zgemmtr,ZGEMMTR)(char *uplo, char *transa, char *transb, blasint *n, blasint *k, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *b, blasint *ldb, blas_complex_double *beta, blas_complex_double *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(zgemmt,ZGEMMT)(char *uplo, char *transa, char *transb, blasint *n, blasint *k, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *b, blasint *ldb, blas_complex_double *beta, blas_complex_double *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

    void FC_GLOBAL(zgemv,ZGEMV)(char *trans, blasint *m, blasint *n, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *x, blasint *incx, blas_complex_double *beta, blas_complex_double *y, blasint *incy, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(zgerc,ZGERC)(blasint *m, blasint *n, blas_complex_double *alpha, blas_complex_double *x, blasint *incx, blas_complex_double *y, blasint *incy, blas_complex_double *a, blasint *lda);

    void FC_GLOBAL(zgeru,ZGERU)(blasint *m, blasint *n, blas_complex_double *alpha, blas_complex_double *x, blasint *incx, blas_complex_double *y, blasint *incy, blas_complex_double *a, blasint *lda);

    void FC_GLOBAL(zhbmv,ZHBMV)(char *uplo, blasint *n, blasint *k, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *x, blasint *incx, blas_complex_double *beta, blas_complex_double *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(zhemm,ZHEMM)(char *side, char *uplo, blasint *m, blasint *n, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *b, blasint *ldb, blas_complex_double *beta, blas_complex_double *c, blasint *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(zhemv,ZHEMV)(char *uplo, blasint *n, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *x, blasint *incx, blas_complex_double *beta, blas_complex_double *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(zher,ZHER)(char *uplo, blasint *n, double *alpha, blas_complex_double *x, blasint *incx, blas_complex_double *a, blasint *lda, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(zher2,ZHER2)(char *uplo, blasint *n, blas_complex_double *alpha, blas_complex_double *x, blasint *incx, blas_complex_double *y, blasint *incy, blas_complex_double *a, blasint *lda, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(zher2k,ZHER2K)(char *uplo, char *trans, blasint *n, blasint *k, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *b, blasint *ldb, double *beta, blas_complex_double *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(zherk,ZHERK)(char *uplo, char *trans, blasint *n, blasint *k, double *alpha, blas_complex_double *a, blasint *lda, double *beta, blas_complex_double *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(zhpmv,ZHPMV)(char *uplo, blasint *n, blas_complex_double *alpha, blas_complex_double *ap, blas_complex_double *x, blasint *incx, blas_complex_double *beta, blas_complex_double *y, blasint *incy, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(zhpr,ZHPR)(char *uplo, blasint *n, double *alpha, blas_complex_double *x, blasint *incx, blas_complex_double *ap, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(zhpr2,ZHPR2)(char *uplo, blasint *n, blas_complex_double *alpha, blas_complex_double *x, blasint *incx, blas_complex_double *y, blasint *incy, blas_complex_double *ap, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(zimatcopy,ZIMATCOPY)(char *ORDER, char *TRANS, blasint *rows, blasint *cols, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blasint *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(zomatcopy,ZOMATCOPY)(char *ORDER, char *TRANS, blasint *rows, blasint *cols, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *b, blasint *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);

    void FC_GLOBAL(zrotg,ZROTG)(blas_complex_double *a, blas_complex_double *b, double *c, blas_complex_double *s);

    void FC_GLOBAL(zscal,ZSCAL)(blasint *n, blas_complex_double *za, blas_complex_double *zx, blasint *incx);

    void FC_GLOBAL(zswap,ZSWAP)(blasint *n, blas_complex_double *zx, blasint *incx, blas_complex_double *zy, blasint *incy);

    void FC_GLOBAL(zsymm,ZSYMM)(char *side, char *uplo, blasint *m, blasint *n, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *b, blasint *ldb, blas_complex_double *beta, blas_complex_double *c, blasint *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

    void FC_GLOBAL(zsyr2k,ZSYR2K)(char *uplo, char *trans, blasint *n, blasint *k, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *b, blasint *ldb, blas_complex_double *beta, blas_complex_double *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(zsyrk,ZSYRK)(char *uplo, char *trans, blasint *n, blasint *k, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *beta, blas_complex_double *c, blasint *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

    void FC_GLOBAL(ztbmv,ZTBMV)(char *uplo, char *trans, char *diag, blasint *n, blasint *k, blas_complex_double *a, blasint *lda, blas_complex_double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ztbsv,ZTBSV)(char *uplo, char *trans, char *diag, blasint *n, blasint *k, blas_complex_double *a, blasint *lda, blas_complex_double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ztpmv,ZTPMV)(char *uplo, char *trans, char *diag, blasint *n, blas_complex_double *ap, blas_complex_double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ztpsv,ZTPSV)(char *uplo, char *trans, char *diag, blasint *n, blas_complex_double *ap, blas_complex_double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ztrmm,ZTRMM)(char *side, char *uplo, char *transa, char *diag, blasint *m, blasint *n, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *b, blasint *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ztrmv,ZTRMV)(char *uplo, char *trans, char *diag, blasint *n, blas_complex_double *a, blasint *lda, blas_complex_double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ztrsm,ZTRSM)(char *side, char *uplo, char *transa, char *diag, blasint *m, blasint *n, blas_complex_double *alpha, blas_complex_double *a, blasint *lda, blas_complex_double *b, blasint *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

    void FC_GLOBAL(ztrsv,ZTRSV)(char *uplo, char *trans, char *diag, blasint *n, blas_complex_double *a, blasint *lda, blas_complex_double *x, blasint *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);



#ifdef __cplusplus
}
#endif
#endif
