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

#ifndef FLEXIBLAS_REAL_BLAS_CALLS_H
#define FLEXIBLAS_REAL_BLAS_CALLS_H

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

void flexiblas_real_caxpby_(void *n, void *ca, void *cx, void *incx, void *cb, void *cy, void *incy);
void flexiblas_real_caxpby(void *n, void *ca, void *cx, void *incx, void *cb, void *cy, void *incy);
void flexiblas_chain_caxpby_(void *n, void *ca, void *cx, void *incx, void *cb, void *cy, void *incy);
void flexiblas_chain_caxpby(void *n, void *ca, void *cx, void *incx, void *cb, void *cy, void *incy);

void flexiblas_real_caxpy_(void *n, void *ca, void *cx, void *incx, void *cy, void *incy);
void flexiblas_real_caxpy(void *n, void *ca, void *cx, void *incx, void *cy, void *incy);
void flexiblas_chain_caxpy_(void *n, void *ca, void *cx, void *incx, void *cy, void *incy);
void flexiblas_chain_caxpy(void *n, void *ca, void *cx, void *incx, void *cy, void *incy);

void flexiblas_real_ccopy_(void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_real_ccopy(void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_chain_ccopy_(void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_chain_ccopy(void *n, void *cx, void *incx, void *cy, void *incy);

void flexiblas_real_cdotc_(blas_complex_float *returnvalue, void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_real_cdotc(blas_complex_float *returnvalue, void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_chain_cdotc_(blas_complex_float *returnvalue, void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_chain_cdotc(blas_complex_float *returnvalue, void *n, void *cx, void *incx, void *cy, void *incy);

void flexiblas_real_cdotu_(blas_complex_float *returnvalue, void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_real_cdotu(blas_complex_float *returnvalue, void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_chain_cdotu_(blas_complex_float *returnvalue, void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_chain_cdotu(blas_complex_float *returnvalue, void *n, void *cx, void *incx, void *cy, void *incy);

void flexiblas_real_cgbmv_(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_cgbmv(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_cgbmv_(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_cgbmv(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_cgeadd_(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_real_cgeadd(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_chain_cgeadd_(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_chain_cgeadd(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);

void flexiblas_real_cgemm_(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_real_cgemm(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_cgemm_(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_cgemm(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

void flexiblas_real_cgemmtr_(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_real_cgemmtr(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_cgemmtr_(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_cgemmtr(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

void flexiblas_real_cgemv_(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_cgemv(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_cgemv_(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_cgemv(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_cgerc_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_real_cgerc(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_cgerc_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_cgerc(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);

void flexiblas_real_cgeru_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_real_cgeru(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_cgeru_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_cgeru(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);

void flexiblas_real_chbmv_(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_chbmv(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chbmv_(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chbmv(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_chemm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_chemm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chemm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chemm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_chemv_(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_chemv(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chemv_(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chemv(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_cher_(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_cher(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_cher_(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_cher(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_cher2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_cher2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_cher2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_cher2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_cher2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_cher2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_cher2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_cher2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_cherk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_cherk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_cherk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_cherk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_chpmv_(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_chpmv(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chpmv_(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chpmv(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_chpr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_chpr(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chpr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chpr(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_chpr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_chpr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chpr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_chpr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_cimatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_cimatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_cimatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_cimatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_comatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_real_comatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_chain_comatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_chain_comatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);

void flexiblas_real_crotg_(void *a, void *b, void *c, void *s);
void flexiblas_real_crotg(void *a, void *b, void *c, void *s);
void flexiblas_chain_crotg_(void *a, void *b, void *c, void *s);
void flexiblas_chain_crotg(void *a, void *b, void *c, void *s);

void flexiblas_real_cscal_(void *n, void *ca, void *cx, void *incx);
void flexiblas_real_cscal(void *n, void *ca, void *cx, void *incx);
void flexiblas_chain_cscal_(void *n, void *ca, void *cx, void *incx);
void flexiblas_chain_cscal(void *n, void *ca, void *cx, void *incx);

void flexiblas_real_csrot_(void *n, void *cx, void *incx, void *cy, void *incy, void *c, void *s);
void flexiblas_real_csrot(void *n, void *cx, void *incx, void *cy, void *incy, void *c, void *s);
void flexiblas_chain_csrot_(void *n, void *cx, void *incx, void *cy, void *incy, void *c, void *s);
void flexiblas_chain_csrot(void *n, void *cx, void *incx, void *cy, void *incy, void *c, void *s);

void flexiblas_real_csscal_(void *n, void *sa, void *cx, void *incx);
void flexiblas_real_csscal(void *n, void *sa, void *cx, void *incx);
void flexiblas_chain_csscal_(void *n, void *sa, void *cx, void *incx);
void flexiblas_chain_csscal(void *n, void *sa, void *cx, void *incx);

void flexiblas_real_cswap_(void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_real_cswap(void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_chain_cswap_(void *n, void *cx, void *incx, void *cy, void *incy);
void flexiblas_chain_cswap(void *n, void *cx, void *incx, void *cy, void *incy);

void flexiblas_real_csymm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_csymm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_csymm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_csymm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_csyr2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_csyr2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_csyr2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_csyr2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_csyrk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_csyrk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_csyrk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_csyrk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_ctbmv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ctbmv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctbmv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctbmv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ctbsv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ctbsv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctbsv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctbsv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ctpmv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ctpmv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctpmv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctpmv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ctpsv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ctpsv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctpsv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctpsv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ctrmm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ctrmm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctrmm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctrmm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ctrmv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ctrmv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctrmv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctrmv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ctrsm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ctrsm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctrsm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctrsm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ctrsv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ctrsv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctrsv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ctrsv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

double flexiblas_real_dasum_(void *n, void *dx, void *incx);
double flexiblas_real_dasum(void *n, void *dx, void *incx);
double flexiblas_chain_dasum_(void *n, void *dx, void *incx);
double flexiblas_chain_dasum(void *n, void *dx, void *incx);

void flexiblas_real_daxpby_(void *n, void *da, void *dx, void *incx, void *db, void *dy, void *incy);
void flexiblas_real_daxpby(void *n, void *da, void *dx, void *incx, void *db, void *dy, void *incy);
void flexiblas_chain_daxpby_(void *n, void *da, void *dx, void *incx, void *db, void *dy, void *incy);
void flexiblas_chain_daxpby(void *n, void *da, void *dx, void *incx, void *db, void *dy, void *incy);

void flexiblas_real_daxpy_(void *n, void *da, void *dx, void *incx, void *dy, void *incy);
void flexiblas_real_daxpy(void *n, void *da, void *dx, void *incx, void *dy, void *incy);
void flexiblas_chain_daxpy_(void *n, void *da, void *dx, void *incx, void *dy, void *incy);
void flexiblas_chain_daxpy(void *n, void *da, void *dx, void *incx, void *dy, void *incy);

void flexiblas_real_dcopy_(void *n, void *dx, void *incx, void *dy, void *incy);
void flexiblas_real_dcopy(void *n, void *dx, void *incx, void *dy, void *incy);
void flexiblas_chain_dcopy_(void *n, void *dx, void *incx, void *dy, void *incy);
void flexiblas_chain_dcopy(void *n, void *dx, void *incx, void *dy, void *incy);

double flexiblas_real_ddot_(void *n, void *dx, void *incx, void *dy, void *incy);
double flexiblas_real_ddot(void *n, void *dx, void *incx, void *dy, void *incy);
double flexiblas_chain_ddot_(void *n, void *dx, void *incx, void *dy, void *incy);
double flexiblas_chain_ddot(void *n, void *dx, void *incx, void *dy, void *incy);

void flexiblas_real_dgbmv_(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_dgbmv(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_dgbmv_(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_dgbmv(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_dgeadd_(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_real_dgeadd(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_chain_dgeadd_(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_chain_dgeadd(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);

void flexiblas_real_dgemm_(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_real_dgemm(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_dgemm_(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_dgemm(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

void flexiblas_real_dgemmtr_(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_real_dgemmtr(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_dgemmtr_(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_dgemmtr(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

void flexiblas_real_dgemv_(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_dgemv(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_dgemv_(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_dgemv(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_dger_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_real_dger(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_dger_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_dger(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);

void flexiblas_real_dimatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_dimatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_dimatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_dimatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);

double flexiblas_real_dnrm2_(void *n, void *x, void *incx);
double flexiblas_real_dnrm2(void *n, void *x, void *incx);
double flexiblas_chain_dnrm2_(void *n, void *x, void *incx);
double flexiblas_chain_dnrm2(void *n, void *x, void *incx);

void flexiblas_real_domatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_real_domatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_chain_domatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_chain_domatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);

void flexiblas_real_drot_(void *n, void *dx, void *incx, void *dy, void *incy, void *c, void *s);
void flexiblas_real_drot(void *n, void *dx, void *incx, void *dy, void *incy, void *c, void *s);
void flexiblas_chain_drot_(void *n, void *dx, void *incx, void *dy, void *incy, void *c, void *s);
void flexiblas_chain_drot(void *n, void *dx, void *incx, void *dy, void *incy, void *c, void *s);

void flexiblas_real_drotg_(void *a, void *b, void *c, void *s);
void flexiblas_real_drotg(void *a, void *b, void *c, void *s);
void flexiblas_chain_drotg_(void *a, void *b, void *c, void *s);
void flexiblas_chain_drotg(void *a, void *b, void *c, void *s);

void flexiblas_real_drotm_(void *n, void *dx, void *incx, void *dy, void *incy, void *dparam);
void flexiblas_real_drotm(void *n, void *dx, void *incx, void *dy, void *incy, void *dparam);
void flexiblas_chain_drotm_(void *n, void *dx, void *incx, void *dy, void *incy, void *dparam);
void flexiblas_chain_drotm(void *n, void *dx, void *incx, void *dy, void *incy, void *dparam);

void flexiblas_real_drotmg_(void *dd1, void *dd2, void *dx1, void *dy1, void *dparam);
void flexiblas_real_drotmg(void *dd1, void *dd2, void *dx1, void *dy1, void *dparam);
void flexiblas_chain_drotmg_(void *dd1, void *dd2, void *dx1, void *dy1, void *dparam);
void flexiblas_chain_drotmg(void *dd1, void *dd2, void *dx1, void *dy1, void *dparam);

void flexiblas_real_dsbmv_(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_dsbmv(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dsbmv_(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dsbmv(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_dscal_(void *n, void *da, void *dx, void *incx);
void flexiblas_real_dscal(void *n, void *da, void *dx, void *incx);
void flexiblas_chain_dscal_(void *n, void *da, void *dx, void *incx);
void flexiblas_chain_dscal(void *n, void *da, void *dx, void *incx);

double flexiblas_real_dsdot_(void *n, void *sx, void *incx, void *sy, void *incy);
double flexiblas_real_dsdot(void *n, void *sx, void *incx, void *sy, void *incy);
double flexiblas_chain_dsdot_(void *n, void *sx, void *incx, void *sy, void *incy);
double flexiblas_chain_dsdot(void *n, void *sx, void *incx, void *sy, void *incy);

void flexiblas_real_dspmv_(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_dspmv(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dspmv_(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dspmv(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_dspr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_dspr(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dspr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dspr(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_dspr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_dspr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dspr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dspr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_dswap_(void *n, void *dx, void *incx, void *dy, void *incy);
void flexiblas_real_dswap(void *n, void *dx, void *incx, void *dy, void *incy);
void flexiblas_chain_dswap_(void *n, void *dx, void *incx, void *dy, void *incy);
void flexiblas_chain_dswap(void *n, void *dx, void *incx, void *dy, void *incy);

void flexiblas_real_dsymm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_dsymm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dsymm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dsymm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_dsymv_(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_dsymv(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dsymv_(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dsymv(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_dsyr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_dsyr(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dsyr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dsyr(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_dsyr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_dsyr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dsyr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_dsyr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_dsyr2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_dsyr2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_dsyr2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_dsyr2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_dsyrk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_dsyrk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_dsyrk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_dsyrk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_dtbmv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_dtbmv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtbmv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtbmv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_dtbsv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_dtbsv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtbsv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtbsv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_dtpmv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_dtpmv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtpmv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtpmv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_dtpsv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_dtpsv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtpsv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtpsv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_dtrmm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_dtrmm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtrmm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtrmm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_dtrmv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_dtrmv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtrmv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtrmv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_dtrsm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_dtrsm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtrsm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtrsm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_dtrsv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_dtrsv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtrsv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_dtrsv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

double flexiblas_real_dzasum_(void *n, void *zx, void *incx);
double flexiblas_real_dzasum(void *n, void *zx, void *incx);
double flexiblas_chain_dzasum_(void *n, void *zx, void *incx);
double flexiblas_chain_dzasum(void *n, void *zx, void *incx);

double flexiblas_real_dznrm2_(void *n, void *x, void *incx);
double flexiblas_real_dznrm2(void *n, void *x, void *incx);
double flexiblas_chain_dznrm2_(void *n, void *x, void *incx);
double flexiblas_chain_dznrm2(void *n, void *x, void *incx);

blasint flexiblas_real_icamax_(void *n, void *cx, void *incx);
blasint flexiblas_real_icamax(void *n, void *cx, void *incx);
blasint flexiblas_chain_icamax_(void *n, void *cx, void *incx);
blasint flexiblas_chain_icamax(void *n, void *cx, void *incx);

blasint flexiblas_real_idamax_(void *n, void *dx, void *incx);
blasint flexiblas_real_idamax(void *n, void *dx, void *incx);
blasint flexiblas_chain_idamax_(void *n, void *dx, void *incx);
blasint flexiblas_chain_idamax(void *n, void *dx, void *incx);

blasint flexiblas_real_isamax_(void *n, void *sx, void *incx);
blasint flexiblas_real_isamax(void *n, void *sx, void *incx);
blasint flexiblas_chain_isamax_(void *n, void *sx, void *incx);
blasint flexiblas_chain_isamax(void *n, void *sx, void *incx);

blasint flexiblas_real_izamax_(void *n, void *zx, void *incx);
blasint flexiblas_real_izamax(void *n, void *zx, void *incx);
blasint flexiblas_chain_izamax_(void *n, void *zx, void *incx);
blasint flexiblas_chain_izamax(void *n, void *zx, void *incx);

float flexiblas_real_sasum_(void *n, void *sx, void *incx);
float flexiblas_real_sasum(void *n, void *sx, void *incx);
float flexiblas_chain_sasum_(void *n, void *sx, void *incx);
float flexiblas_chain_sasum(void *n, void *sx, void *incx);

void flexiblas_real_saxpby_(void *n, void *sa, void *sx, void *incx, void *sb, void *sy, void *incy);
void flexiblas_real_saxpby(void *n, void *sa, void *sx, void *incx, void *sb, void *sy, void *incy);
void flexiblas_chain_saxpby_(void *n, void *sa, void *sx, void *incx, void *sb, void *sy, void *incy);
void flexiblas_chain_saxpby(void *n, void *sa, void *sx, void *incx, void *sb, void *sy, void *incy);

void flexiblas_real_saxpy_(void *n, void *sa, void *sx, void *incx, void *sy, void *incy);
void flexiblas_real_saxpy(void *n, void *sa, void *sx, void *incx, void *sy, void *incy);
void flexiblas_chain_saxpy_(void *n, void *sa, void *sx, void *incx, void *sy, void *incy);
void flexiblas_chain_saxpy(void *n, void *sa, void *sx, void *incx, void *sy, void *incy);

float flexiblas_real_scasum_(void *n, void *cx, void *incx);
float flexiblas_real_scasum(void *n, void *cx, void *incx);
float flexiblas_chain_scasum_(void *n, void *cx, void *incx);
float flexiblas_chain_scasum(void *n, void *cx, void *incx);

float flexiblas_real_scnrm2_(void *n, void *x, void *incx);
float flexiblas_real_scnrm2(void *n, void *x, void *incx);
float flexiblas_chain_scnrm2_(void *n, void *x, void *incx);
float flexiblas_chain_scnrm2(void *n, void *x, void *incx);

void flexiblas_real_scopy_(void *n, void *sx, void *incx, void *sy, void *incy);
void flexiblas_real_scopy(void *n, void *sx, void *incx, void *sy, void *incy);
void flexiblas_chain_scopy_(void *n, void *sx, void *incx, void *sy, void *incy);
void flexiblas_chain_scopy(void *n, void *sx, void *incx, void *sy, void *incy);

float flexiblas_real_sdot_(void *n, void *sx, void *incx, void *sy, void *incy);
float flexiblas_real_sdot(void *n, void *sx, void *incx, void *sy, void *incy);
float flexiblas_chain_sdot_(void *n, void *sx, void *incx, void *sy, void *incy);
float flexiblas_chain_sdot(void *n, void *sx, void *incx, void *sy, void *incy);

float flexiblas_real_sdsdot_(void *n, void *sb, void *sx, void *incx, void *sy, void *incy);
float flexiblas_real_sdsdot(void *n, void *sb, void *sx, void *incx, void *sy, void *incy);
float flexiblas_chain_sdsdot_(void *n, void *sb, void *sx, void *incx, void *sy, void *incy);
float flexiblas_chain_sdsdot(void *n, void *sb, void *sx, void *incx, void *sy, void *incy);

void flexiblas_real_sgbmv_(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_sgbmv(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_sgbmv_(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_sgbmv(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_sgeadd_(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_real_sgeadd(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_chain_sgeadd_(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_chain_sgeadd(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);

void flexiblas_real_sgemm_(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_real_sgemm(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_sgemm_(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_sgemm(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

void flexiblas_real_sgemmtr_(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_real_sgemmtr(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_sgemmtr_(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_sgemmtr(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

void flexiblas_real_sgemv_(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_sgemv(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_sgemv_(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_sgemv(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_sger_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_real_sger(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_sger_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_sger(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);

void flexiblas_real_simatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_simatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_simatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_simatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);

float flexiblas_real_snrm2_(void *n, void *x, void *incx);
float flexiblas_real_snrm2(void *n, void *x, void *incx);
float flexiblas_chain_snrm2_(void *n, void *x, void *incx);
float flexiblas_chain_snrm2(void *n, void *x, void *incx);

void flexiblas_real_somatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_real_somatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_chain_somatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_chain_somatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);

void flexiblas_real_srot_(void *n, void *sx, void *incx, void *sy, void *incy, void *c, void *s);
void flexiblas_real_srot(void *n, void *sx, void *incx, void *sy, void *incy, void *c, void *s);
void flexiblas_chain_srot_(void *n, void *sx, void *incx, void *sy, void *incy, void *c, void *s);
void flexiblas_chain_srot(void *n, void *sx, void *incx, void *sy, void *incy, void *c, void *s);

void flexiblas_real_srotg_(void *a, void *b, void *c, void *s);
void flexiblas_real_srotg(void *a, void *b, void *c, void *s);
void flexiblas_chain_srotg_(void *a, void *b, void *c, void *s);
void flexiblas_chain_srotg(void *a, void *b, void *c, void *s);

void flexiblas_real_srotm_(void *n, void *sx, void *incx, void *sy, void *incy, void *sparam);
void flexiblas_real_srotm(void *n, void *sx, void *incx, void *sy, void *incy, void *sparam);
void flexiblas_chain_srotm_(void *n, void *sx, void *incx, void *sy, void *incy, void *sparam);
void flexiblas_chain_srotm(void *n, void *sx, void *incx, void *sy, void *incy, void *sparam);

void flexiblas_real_srotmg_(void *sd1, void *sd2, void *sx1, void *sy1, void *sparam);
void flexiblas_real_srotmg(void *sd1, void *sd2, void *sx1, void *sy1, void *sparam);
void flexiblas_chain_srotmg_(void *sd1, void *sd2, void *sx1, void *sy1, void *sparam);
void flexiblas_chain_srotmg(void *sd1, void *sd2, void *sx1, void *sy1, void *sparam);

void flexiblas_real_ssbmv_(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_ssbmv(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_ssbmv_(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_ssbmv(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_sscal_(void *n, void *sa, void *sx, void *incx);
void flexiblas_real_sscal(void *n, void *sa, void *sx, void *incx);
void flexiblas_chain_sscal_(void *n, void *sa, void *sx, void *incx);
void flexiblas_chain_sscal(void *n, void *sa, void *sx, void *incx);

void flexiblas_real_sspmv_(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_sspmv(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_sspmv_(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_sspmv(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_sspr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_sspr(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_sspr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_sspr(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_sspr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_sspr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_sspr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_sspr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_sswap_(void *n, void *sx, void *incx, void *sy, void *incy);
void flexiblas_real_sswap(void *n, void *sx, void *incx, void *sy, void *incy);
void flexiblas_chain_sswap_(void *n, void *sx, void *incx, void *sy, void *incy);
void flexiblas_chain_sswap(void *n, void *sx, void *incx, void *sy, void *incy);

void flexiblas_real_ssymm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_ssymm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_ssymm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_ssymm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_ssymv_(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_ssymv(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_ssymv_(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_ssymv(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_ssyr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_ssyr(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_ssyr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_ssyr(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_ssyr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_ssyr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_ssyr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_ssyr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_ssyr2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_ssyr2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_ssyr2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_ssyr2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_ssyrk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_ssyrk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_ssyrk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_ssyrk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_stbmv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_stbmv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_stbmv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_stbmv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_stbsv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_stbsv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_stbsv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_stbsv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_stpmv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_stpmv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_stpmv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_stpmv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_stpsv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_stpsv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_stpsv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_stpsv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_strmm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_strmm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_strmm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_strmm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_strmv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_strmv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_strmv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_strmv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_strsm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_strsm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_strsm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_strsm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_strsv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_strsv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_strsv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_strsv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_zaxpby_(void *n, void *za, void *zx, void *incx, void *zb, void *zy, void *incy);
void flexiblas_real_zaxpby(void *n, void *za, void *zx, void *incx, void *zb, void *zy, void *incy);
void flexiblas_chain_zaxpby_(void *n, void *za, void *zx, void *incx, void *zb, void *zy, void *incy);
void flexiblas_chain_zaxpby(void *n, void *za, void *zx, void *incx, void *zb, void *zy, void *incy);

void flexiblas_real_zaxpy_(void *n, void *za, void *zx, void *incx, void *zy, void *incy);
void flexiblas_real_zaxpy(void *n, void *za, void *zx, void *incx, void *zy, void *incy);
void flexiblas_chain_zaxpy_(void *n, void *za, void *zx, void *incx, void *zy, void *incy);
void flexiblas_chain_zaxpy(void *n, void *za, void *zx, void *incx, void *zy, void *incy);

void flexiblas_real_zcopy_(void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_real_zcopy(void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_chain_zcopy_(void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_chain_zcopy(void *n, void *zx, void *incx, void *zy, void *incy);

void flexiblas_real_zdotc_(blas_complex_double *returnvalue, void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_real_zdotc(blas_complex_double *returnvalue, void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_chain_zdotc_(blas_complex_double *returnvalue, void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_chain_zdotc(blas_complex_double *returnvalue, void *n, void *zx, void *incx, void *zy, void *incy);

void flexiblas_real_zdotu_(blas_complex_double *returnvalue, void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_real_zdotu(blas_complex_double *returnvalue, void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_chain_zdotu_(blas_complex_double *returnvalue, void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_chain_zdotu(blas_complex_double *returnvalue, void *n, void *zx, void *incx, void *zy, void *incy);

void flexiblas_real_zdrot_(void *n, void *zx, void *incx, void *zy, void *incy, void *c, void *s);
void flexiblas_real_zdrot(void *n, void *zx, void *incx, void *zy, void *incy, void *c, void *s);
void flexiblas_chain_zdrot_(void *n, void *zx, void *incx, void *zy, void *incy, void *c, void *s);
void flexiblas_chain_zdrot(void *n, void *zx, void *incx, void *zy, void *incy, void *c, void *s);

void flexiblas_real_zdscal_(void *n, void *da, void *zx, void *incx);
void flexiblas_real_zdscal(void *n, void *da, void *zx, void *incx);
void flexiblas_chain_zdscal_(void *n, void *da, void *zx, void *incx);
void flexiblas_chain_zdscal(void *n, void *da, void *zx, void *incx);

void flexiblas_real_zgbmv_(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_zgbmv(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zgbmv_(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zgbmv(void *trans, void *m, void *n, void *kl, void *ku, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_zgeadd_(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_real_zgeadd(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_chain_zgeadd_(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);
void flexiblas_chain_zgeadd(void *m, void *n, void *alpha, void *a, void *lda, void *beta, void *b, void *ldb);

void flexiblas_real_zgemm_(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_real_zgemm(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_zgemm_(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_zgemm(void *transa, void *transb, void *m, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

void flexiblas_real_zgemmtr_(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_real_zgemmtr(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_zgemmtr_(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);
void flexiblas_chain_zgemmtr(void *uplo, void *transa, void *transb, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t transb_len);

void flexiblas_real_zgemv_(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_zgemv(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zgemv_(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zgemv(void *trans, void *m, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_zgerc_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_real_zgerc(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_zgerc_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_zgerc(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);

void flexiblas_real_zgeru_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_real_zgeru(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_zgeru_(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);
void flexiblas_chain_zgeru(void *m, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda);

void flexiblas_real_zhbmv_(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_zhbmv(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhbmv_(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhbmv(void *uplo, void *n, void *k, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_zhemm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_zhemm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhemm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhemm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_zhemv_(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_zhemv(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhemv_(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhemv(void *uplo, void *n, void *alpha, void *a, void *lda, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_zher_(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_zher(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zher_(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zher(void *uplo, void *n, void *alpha, void *x, void *incx, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_zher2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_zher2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zher2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zher2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *a, void *lda, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_zher2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_zher2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zher2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zher2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_zherk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_zherk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zherk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zherk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_zhpmv_(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_zhpmv(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhpmv_(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhpmv(void *uplo, void *n, void *alpha, void *ap, void *x, void *incx, void *beta, void *y, void *incy, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_zhpr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_zhpr(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhpr_(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhpr(void *uplo, void *n, void *alpha, void *x, void *incx, void *ap, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_zhpr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_zhpr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhpr2_(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zhpr2(void *uplo, void *n, void *alpha, void *x, void *incx, void *y, void *incy, void *ap, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_zimatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_zimatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zimatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zimatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *ldb, flexiblas_fortran_charlen_t order_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_zomatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_real_zomatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_chain_zomatcopy_(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);
void flexiblas_chain_zomatcopy(void *ORDER, void *TRANS, void *rows, void *cols, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t ORDER_len, flexiblas_fortran_charlen_t TRANS_len);

void flexiblas_real_zrotg_(void *a, void *b, void *c, void *s);
void flexiblas_real_zrotg(void *a, void *b, void *c, void *s);
void flexiblas_chain_zrotg_(void *a, void *b, void *c, void *s);
void flexiblas_chain_zrotg(void *a, void *b, void *c, void *s);

void flexiblas_real_zscal_(void *n, void *za, void *zx, void *incx);
void flexiblas_real_zscal(void *n, void *za, void *zx, void *incx);
void flexiblas_chain_zscal_(void *n, void *za, void *zx, void *incx);
void flexiblas_chain_zscal(void *n, void *za, void *zx, void *incx);

void flexiblas_real_zswap_(void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_real_zswap(void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_chain_zswap_(void *n, void *zx, void *incx, void *zy, void *incy);
void flexiblas_chain_zswap(void *n, void *zx, void *incx, void *zy, void *incy);

void flexiblas_real_zsymm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_real_zsymm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zsymm_(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);
void flexiblas_chain_zsymm(void *side, void *uplo, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len);

void flexiblas_real_zsyr2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_zsyr2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zsyr2k_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zsyr2k(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *b, void *ldb, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_zsyrk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_real_zsyrk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zsyrk_(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);
void flexiblas_chain_zsyrk(void *uplo, void *trans, void *n, void *k, void *alpha, void *a, void *lda, void *beta, void *c, void *ldc, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len);

void flexiblas_real_ztbmv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ztbmv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztbmv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztbmv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ztbsv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ztbsv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztbsv_(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztbsv(void *uplo, void *trans, void *diag, void *n, void *k, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ztpmv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ztpmv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztpmv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztpmv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ztpsv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ztpsv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztpsv_(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztpsv(void *uplo, void *trans, void *diag, void *n, void *ap, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ztrmm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ztrmm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztrmm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztrmm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ztrmv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ztrmv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztrmv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztrmv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ztrsm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ztrsm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztrsm_(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztrsm(void *side, void *uplo, void *transa, void *diag, void *m, void *n, void *alpha, void *a, void *lda, void *b, void *ldb, flexiblas_fortran_charlen_t side_len, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t transa_len, flexiblas_fortran_charlen_t diag_len);

void flexiblas_real_ztrsv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_real_ztrsv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztrsv_(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);
void flexiblas_chain_ztrsv(void *uplo, void *trans, void *diag, void *n, void *a, void *lda, void *x, void *incx, flexiblas_fortran_charlen_t uplo_len, flexiblas_fortran_charlen_t trans_len, flexiblas_fortran_charlen_t diag_len);



#ifdef __cplusplus
}
#endif
#endif
