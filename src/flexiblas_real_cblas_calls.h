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

#ifndef FLEXIBLAS_REAL_BLAS_CALLS_CBLAS_H
#define FLEXIBLAS_REAL_BLAS_CALLS_CBLAS_H

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

void flexiblas_real_cblas_caxpby(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_caxpby(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_caxpy(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_caxpy(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_ccopy(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_ccopy(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_cdotc_sub(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);
void flexiblas_chain_cblas_cdotc_sub(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);

void flexiblas_real_cblas_cdotu_sub(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);
void flexiblas_chain_cblas_cdotu_sub(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);

void flexiblas_real_cblas_cgbmv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_cgbmv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_cgeadd(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);
void flexiblas_chain_cblas_cgeadd(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);

void flexiblas_real_cblas_cgemm(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_cgemm(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_cgemmtr(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_cgemmtr(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_cgemv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_cgemv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_cgerc(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_cgerc(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_cgeru(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_cgeru(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_chbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_chbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_chemm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_chemm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_chemv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_chemv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_cher(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_cher(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_cher2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_cher2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_cher2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_cher2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_cherk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_cherk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_chpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_chpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_chpr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A);
void flexiblas_chain_cblas_chpr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A);

void flexiblas_real_cblas_chpr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);
void flexiblas_chain_cblas_chpr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);

void flexiblas_real_cblas_cimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
void flexiblas_chain_cblas_cimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);

void flexiblas_real_cblas_comatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
void flexiblas_chain_cblas_comatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);

void flexiblas_real_cblas_crotg(void *a, void *b, void *c, void *s);
void flexiblas_chain_cblas_crotg(void *a, void *b, void *c, void *s);

void flexiblas_real_cblas_cscal(const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_cscal(const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_csrot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);
void flexiblas_chain_cblas_csrot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);

void flexiblas_real_cblas_csscal(const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_csscal(const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_cswap(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_cswap(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_csymm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_csymm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_csyr2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_csyr2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_csyrk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_csyrk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_ctbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ctbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_ctbsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ctbsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_ctpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ctpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_ctpsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ctpsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_ctrmm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
void flexiblas_chain_cblas_ctrmm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);

void flexiblas_real_cblas_ctrmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ctrmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_ctrsm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
void flexiblas_chain_cblas_ctrsm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);

void flexiblas_real_cblas_ctrsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ctrsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

double flexiblas_real_cblas_dasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
double flexiblas_chain_cblas_dasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_daxpby(const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_daxpby(const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_daxpy(const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_daxpy(const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_dcopy(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_dcopy(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

double flexiblas_real_cblas_ddot(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
double flexiblas_chain_cblas_ddot(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_dgbmv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_dgbmv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_dgeadd(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const double cbeta, void *b, const CBLAS_INT cldb);
void flexiblas_chain_cblas_dgeadd(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const double cbeta, void *b, const CBLAS_INT cldb);

void flexiblas_real_cblas_dgemm(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_dgemm(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_dgemmtr(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_dgemmtr(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_dgemv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_dgemv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_dger(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_dger(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_dimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
void flexiblas_chain_cblas_dimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);

double flexiblas_real_cblas_dnrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
double flexiblas_chain_cblas_dnrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_domatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
void flexiblas_chain_cblas_domatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);

void flexiblas_real_cblas_drot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);
void flexiblas_chain_cblas_drot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);

void flexiblas_real_cblas_drotg(void *a, void *b, void *c, void *s);
void flexiblas_chain_cblas_drotg(void *a, void *b, void *c, void *s);

void flexiblas_real_cblas_drotm(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);
void flexiblas_chain_cblas_drotm(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);

void flexiblas_real_cblas_drotmg(void *d1, void *d2, void *b1, const double b2, void *P);
void flexiblas_chain_cblas_drotmg(void *d1, void *d2, void *b1, const double b2, void *P);

void flexiblas_real_cblas_dsbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_dsbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_dscal(const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_dscal(const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);

double flexiblas_real_cblas_dsdot(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
double flexiblas_chain_cblas_dsdot(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_dspmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *Ap, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_dspmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *Ap, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_dspr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Ap);
void flexiblas_chain_cblas_dspr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Ap);

void flexiblas_real_cblas_dspr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);
void flexiblas_chain_cblas_dspr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);

void flexiblas_real_cblas_dswap(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_dswap(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_dsymm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_dsymm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_dsymv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_dsymv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_dsyr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_dsyr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_dsyr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_dsyr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_dsyr2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_dsyr2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_dsyrk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_dsyrk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_dtbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_dtbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_dtbsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_dtbsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_dtpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_dtpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_dtpsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_dtpsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_dtrmm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
void flexiblas_chain_cblas_dtrmm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);

void flexiblas_real_cblas_dtrmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_dtrmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_dtrsm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
void flexiblas_chain_cblas_dtrsm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);

void flexiblas_real_cblas_dtrsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_dtrsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

double flexiblas_real_cblas_dzasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
double flexiblas_chain_cblas_dzasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

double flexiblas_real_cblas_dznrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
double flexiblas_chain_cblas_dznrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

size_t flexiblas_real_cblas_icamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
size_t flexiblas_chain_cblas_icamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

size_t flexiblas_real_cblas_idamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
size_t flexiblas_chain_cblas_idamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

size_t flexiblas_real_cblas_isamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
size_t flexiblas_chain_cblas_isamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

size_t flexiblas_real_cblas_izamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
size_t flexiblas_chain_cblas_izamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

float flexiblas_real_cblas_sasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
float flexiblas_chain_cblas_sasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_saxpby(const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_saxpby(const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_saxpy(const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_saxpy(const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

float flexiblas_real_cblas_scasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
float flexiblas_chain_cblas_scasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

float flexiblas_real_cblas_scnrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
float flexiblas_chain_cblas_scnrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_scopy(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_scopy(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

float flexiblas_real_cblas_sdot(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
float flexiblas_chain_cblas_sdot(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);

float flexiblas_real_cblas_sdsdot(const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
float flexiblas_chain_cblas_sdsdot(const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_sgbmv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_sgbmv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_sgeadd(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const float cbeta, void *b, const CBLAS_INT cldb);
void flexiblas_chain_cblas_sgeadd(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const float cbeta, void *b, const CBLAS_INT cldb);

void flexiblas_real_cblas_sgemm(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_sgemm(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_sgemmtr(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_sgemmtr(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_sgemv(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_sgemv(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_sger(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_sger(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_simatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
void flexiblas_chain_cblas_simatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);

float flexiblas_real_cblas_snrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX);
float flexiblas_chain_cblas_snrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_somatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
void flexiblas_chain_cblas_somatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);

void flexiblas_real_cblas_srot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);
void flexiblas_chain_cblas_srot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);

void flexiblas_real_cblas_srotg(void *a, void *b, void *c, void *s);
void flexiblas_chain_cblas_srotg(void *a, void *b, void *c, void *s);

void flexiblas_real_cblas_srotm(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);
void flexiblas_chain_cblas_srotm(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);

void flexiblas_real_cblas_srotmg(void *d1, void *d2, void *b1, const float b2, void *P);
void flexiblas_chain_cblas_srotmg(void *d1, void *d2, void *b1, const float b2, void *P);

void flexiblas_real_cblas_ssbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_ssbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_sscal(const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_sscal(const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_sspmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *Ap, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_sspmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *Ap, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_sspr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Ap);
void flexiblas_chain_cblas_sspr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Ap);

void flexiblas_real_cblas_sspr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);
void flexiblas_chain_cblas_sspr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);

void flexiblas_real_cblas_sswap(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_sswap(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_ssymm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_ssymm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_ssymv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_ssymv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_ssyr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_ssyr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_ssyr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_ssyr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_ssyr2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_ssyr2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_ssyrk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_ssyrk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_stbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_stbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_stbsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_stbsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_stpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_stpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_stpsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_stpsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_strmm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
void flexiblas_chain_cblas_strmm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);

void flexiblas_real_cblas_strmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_strmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_strsm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
void flexiblas_chain_cblas_strsm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);

void flexiblas_real_cblas_strsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_strsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_zaxpby(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_zaxpby(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_zaxpy(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_zaxpy(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_zcopy(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_zcopy(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_zdotc_sub(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);
void flexiblas_chain_cblas_zdotc_sub(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);

void flexiblas_real_cblas_zdotu_sub(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);
void flexiblas_chain_cblas_zdotu_sub(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);

void flexiblas_real_cblas_zdrot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);
void flexiblas_chain_cblas_zdrot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);

void flexiblas_real_cblas_zdscal(const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_zdscal(const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_zgbmv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_zgbmv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_zgeadd(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);
void flexiblas_chain_cblas_zgeadd(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);

void flexiblas_real_cblas_zgemm(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_zgemm(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_zgemmtr(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_zgemmtr(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_zgemv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_zgemv(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_zgerc(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_zgerc(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_zgeru(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_zgeru(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_zhbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_zhbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_zhemm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_zhemm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_zhemv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_zhemv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_zher(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_zher(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_zher2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
void flexiblas_chain_cblas_zher2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);

void flexiblas_real_cblas_zher2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_zher2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_zherk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_zherk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_zhpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_zhpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_zhpr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A);
void flexiblas_chain_cblas_zhpr(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A);

void flexiblas_real_cblas_zhpr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);
void flexiblas_chain_cblas_zhpr2(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);

void flexiblas_real_cblas_zimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
void flexiblas_chain_cblas_zimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);

void flexiblas_real_cblas_zomatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
void flexiblas_chain_cblas_zomatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);

void flexiblas_real_cblas_zrotg(void *a, void *b, void *c, void *s);
void flexiblas_chain_cblas_zrotg(void *a, void *b, void *c, void *s);

void flexiblas_real_cblas_zscal(const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_zscal(const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_zswap(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
void flexiblas_chain_cblas_zswap(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

void flexiblas_real_cblas_zsymm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_zsymm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_zsyr2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_zsyr2k(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_zsyrk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);
void flexiblas_chain_cblas_zsyrk(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);

void flexiblas_real_cblas_ztbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ztbmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_ztbsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ztbsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_ztpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ztpmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_ztpsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ztpsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_ztrmm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
void flexiblas_chain_cblas_ztrmm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);

void flexiblas_real_cblas_ztrmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ztrmv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);

void flexiblas_real_cblas_ztrsm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
void flexiblas_chain_cblas_ztrsm(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);

void flexiblas_real_cblas_ztrsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
void flexiblas_chain_cblas_ztrsv(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);



#ifdef __cplusplus
}
#endif
#endif
