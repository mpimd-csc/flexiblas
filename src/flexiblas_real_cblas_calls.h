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



#ifndef FLEXIBLAS_REAL_CALLS_H
#define FLEXIBLAS_REAL_CALLS_H

#include <stdint.h>
#include <complex.h>

#ifdef __cplusplus
extern "C" {
#endif

    void flexiblas_real_cblas_caxpy( const int N, const void *alpha, const void *X, const int incX, void *Y, const int incY);
    void flexiblas_chain_cblas_caxpy( const int N, const void *alpha, const void *X, const int incX, void *Y, const int incY);
    void flexiblas_real_cblas_ccopy( const int N, const void *X,const int incX, void *Y, const int incY);
    void flexiblas_chain_cblas_ccopy( const int N, const void *X,const int incX, void *Y, const int incY);
    void flexiblas_real_cblas_cdotc_sub( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc);
    void flexiblas_chain_cblas_cdotc_sub( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc);
    void flexiblas_real_cblas_cdotu_sub( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc);
    void flexiblas_chain_cblas_cdotu_sub( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc);

    void flexiblas_real_cblas_crotg(  void *a, void *b, float *c, void *s);
    void flexiblas_chain_cblas_crotg(  void *a, void *b,float  *c, void *s);
    void flexiblas_real_cblas_zrotg(  void *a, void *b, double *c, void *s);
    void flexiblas_chain_cblas_zrotg(  void *a, void *b, double *c, void *s);

    void flexiblas_real_cblas_csrot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);
    void flexiblas_chain_cblas_csrot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);

    void flexiblas_real_cblas_zdrot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);
    void flexiblas_chain_cblas_zdrot(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);


    void flexiblas_real_cblas_cgbmv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const int KL, const int KU,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_chain_cblas_cgbmv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const int KL, const int KU,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_real_cblas_cgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const int M, const int N,
        const int K, const void *alpha, const void  *A,
        const int lda, const void  *B, const int ldb,
        const void *beta, void  *C, const int ldc);
    void flexiblas_chain_cblas_cgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const int M, const int N,
        const int K, const void *alpha, const void  *A,
        const int lda, const void  *B, const int ldb,
        const void *beta, void  *C, const int ldc);
    void flexiblas_real_cblas_cgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_chain_cblas_cgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_real_cblas_cgerc(const CBLAS_LAYOUT layout, const int M, const int N,
        const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_chain_cblas_cgerc(const CBLAS_LAYOUT layout, const int M, const int N,
        const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_real_cblas_cgeru(const CBLAS_LAYOUT layout, const int M, const int N,
        const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_chain_cblas_cgeru(const CBLAS_LAYOUT layout, const int M, const int N,
        const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_real_cblas_chbmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo,const int N,const int K,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_chain_cblas_chbmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo,const int N,const int K,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_real_cblas_chemm(const CBLAS_LAYOUT layout, const  CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const int M, const int N,
        const void *alpha, const void *A, const int lda,
        const void *B, const int ldb, const void *beta,
        void *C, const int ldc);
    void flexiblas_chain_cblas_chemm(const CBLAS_LAYOUT layout, const  CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const int M, const int N,
        const void *alpha, const void *A, const int lda,
        const void *B, const int ldb, const void *beta,
        void *C, const int ldc);
    void flexiblas_real_cblas_chemv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const void *alpha, const void *A, const int lda,
        const void *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_chain_cblas_chemv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const void *alpha, const void *A, const int lda,
        const void *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_real_cblas_cher(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const float alpha, const void *X, const int incX
        ,void *A, const int lda);
    void flexiblas_chain_cblas_cher(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const float alpha, const void *X, const int incX
        ,void *A, const int lda);
    void flexiblas_real_cblas_cher2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_chain_cblas_cher2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_real_cblas_cher2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void *A, const int lda,
        const void *B, const int ldb, const float beta,
        void *C, const int ldc);
    void flexiblas_chain_cblas_cher2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void *A, const int lda,
        const void *B, const int ldb, const float beta,
        void *C, const int ldc);
    void flexiblas_real_cblas_cherk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const float alpha, const void *A, const int lda,
        const float beta, void *C, const int ldc);
    void flexiblas_chain_cblas_cherk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const float alpha, const void *A, const int lda,
        const float beta, void *C, const int ldc);
    void flexiblas_real_cblas_chpmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo,const int N,
        const void *alpha, const void  *AP,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_chain_cblas_chpmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo,const int N,
        const void *alpha, const void  *AP,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_real_cblas_chpr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const float alpha, const void *X,
        const int incX, void *A);
    void flexiblas_chain_cblas_chpr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const float alpha, const void *X,
        const int incX, void *A);
    void flexiblas_chain_cblas_chpr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N,const void *alpha, const void *X,
        const int incX,const void *Y, const int incY, void *Ap);
    void flexiblas_real_cblas_chpr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N,const void *alpha, const void *X,
        const int incX,const void *Y, const int incY, void *Ap);
    void flexiblas_real_cblas_cscal( const int N, const void *alpha, void *X,
        const int incX);
    void flexiblas_chain_cblas_cscal( const int N, const void *alpha, void *X,
        const int incX);
    void flexiblas_real_cblas_csscal( const int N, const float alpha, void *X,
                       const int incX);
    void flexiblas_chain_cblas_csscal( const int N, const float alpha, void *X,
                       const int incX);
    void flexiblas_real_cblas_cswap( const int N, void *X, const int incX, void *Y,
                       const int incY);
    void flexiblas_chain_cblas_cswap( const int N, void *X, const int incX, void *Y,
                       const int incY);
    void flexiblas_real_cblas_csymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        const void  *B, const int ldb, const void *beta,
        void  *C, const int ldc);
    void flexiblas_chain_cblas_csymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        const void  *B, const int ldb, const void *beta,
        void  *C, const int ldc);
    void flexiblas_real_cblas_csyr2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void  *B, const int ldb, const void *beta,
        void  *C, const int ldc);
    void flexiblas_chain_cblas_csyr2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void  *B, const int ldb, const void *beta,
        void  *C, const int ldc);
    void flexiblas_real_cblas_csyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void *beta, void  *C, const int ldc);
    void flexiblas_chain_cblas_csyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void *beta, void  *C, const int ldc);
    void flexiblas_real_cblas_csyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void *beta, void  *C, const int ldc);
    void flexiblas_chain_cblas_csyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void *beta, void  *C, const int ldc);
    void flexiblas_real_cblas_ctbmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_chain_cblas_ctbmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_real_cblas_ctbsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_chain_cblas_ctbsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_chain_cblas_ctpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *Ap, void  *X, const int incX);
    void flexiblas_real_cblas_ctpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *Ap, void  *X, const int incX);
    void flexiblas_chain_cblas_ctpsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *Ap, void  *X, const int incX);
    void flexiblas_real_cblas_ctpsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *Ap, void  *X, const int incX);
    void flexiblas_chain_cblas_ctrmm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        void  *B, const int ldb);
    void flexiblas_real_cblas_ctrmm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        void  *B, const int ldb);
    void flexiblas_real_cblas_ctrmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_chain_cblas_ctrmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_chain_cblas_ctrsm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        void  *B, const int ldb);
    void flexiblas_real_cblas_ctrsm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        void  *B, const int ldb);
    void flexiblas_real_cblas_ctrsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *A, const int lda, void  *X,
        const int incX);
    void flexiblas_chain_cblas_ctrsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *A, const int lda, void  *X,
        const int incX);


    /* Complex 16   */
    void flexiblas_real_cblas_zaxpy( const int N, const void *alpha, const void *X, const int incX, void *Y, const int incY);
    void flexiblas_chain_cblas_zaxpy( const int N, const void *alpha, const void *X, const int incX, void *Y, const int incY);
    void flexiblas_real_cblas_zcopy( const int N, const void *X,const int incX, void *Y, const int incY);
    void flexiblas_chain_cblas_zcopy( const int N, const void *X,const int incX, void *Y, const int incY);
    void flexiblas_real_cblas_zdotc_sub( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc);
    void flexiblas_chain_cblas_zdotc_sub( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc);
    void flexiblas_real_cblas_zdotu_sub( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc);
    void flexiblas_chain_cblas_zdotu_sub( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc);
    void flexiblas_real_cblas_zgbmv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const int KL, const int KU,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_chain_cblas_zgbmv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const int KL, const int KU,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_real_cblas_zgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const int M, const int N,
        const int K, const void *alpha, const void  *A,
        const int lda, const void  *B, const int ldb,
        const void *beta, void  *C, const int ldc);
    void flexiblas_chain_cblas_zgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const int M, const int N,
        const int K, const void *alpha, const void  *A,
        const int lda, const void  *B, const int ldb,
        const void *beta, void  *C, const int ldc);
    void flexiblas_real_cblas_zgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_chain_cblas_zgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_real_cblas_zgerc(const CBLAS_LAYOUT layout, const int M, const int N,
        const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_chain_cblas_zgerc(const CBLAS_LAYOUT layout, const int M, const int N,
        const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_real_cblas_zgeru(const CBLAS_LAYOUT layout, const int M, const int N,
        const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_chain_cblas_zgeru(const CBLAS_LAYOUT layout, const int M, const int N,
        const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_real_cblas_zhbmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo,const int N,const int K,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_chain_cblas_zhbmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo,const int N,const int K,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_real_cblas_zhemm(const CBLAS_LAYOUT layout, const  CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const int M, const int N,
        const void *alpha, const void *A, const int lda,
        const void *B, const int ldb, const void *beta,
        void *C, const int ldc);
    void flexiblas_chain_cblas_zhemm(const CBLAS_LAYOUT layout, const  CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const int M, const int N,
        const void *alpha, const void *A, const int lda,
        const void *B, const int ldb, const void *beta,
        void *C, const int ldc);
    void flexiblas_real_cblas_zhemv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const void *alpha, const void *A, const int lda,
        const void *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_chain_cblas_zhemv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const void *alpha, const void *A, const int lda,
        const void *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_real_cblas_zher(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const double alpha, const void *X, const int incX
        ,void *A, const int lda);
    void flexiblas_chain_cblas_zher(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const double alpha, const void *X, const int incX
        ,void *A, const int lda);
    void flexiblas_real_cblas_zher2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_chain_cblas_zher2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda);
    void flexiblas_real_cblas_zher2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void *A, const int lda,
        const void *B, const int ldb, const double beta,
        void *C, const int ldc);
    void flexiblas_chain_cblas_zher2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void *A, const int lda,
        const void *B, const int ldb, const double beta,
        void *C, const int ldc);
    void flexiblas_real_cblas_zherk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const double alpha, const void *A, const int lda,
        const double beta, void *C, const int ldc);
    void flexiblas_chain_cblas_zherk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const double alpha, const void *A, const int lda,
        const double beta, void *C, const int ldc);
    void flexiblas_real_cblas_zhpmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo,const int N,
        const void *alpha, const void  *AP,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_chain_cblas_zhpmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo,const int N,
        const void *alpha, const void  *AP,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY);
    void flexiblas_real_cblas_zhpr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const double alpha, const void *X,
        const int incX, void *A);
    void flexiblas_chain_cblas_zhpr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const double alpha, const void *X,
        const int incX, void *A);
    void flexiblas_chain_cblas_zhpr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N,const void *alpha, const void *X,
        const int incX,const void *Y, const int incY, void *Ap);
    void flexiblas_real_cblas_zhpr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N,const void *alpha, const void *X,
        const int incX,const void *Y, const int incY, void *Ap);
    void flexiblas_real_cblas_zscal( const int N, const void *alpha, void *X,
        const int incX);
    void flexiblas_chain_cblas_zscal( const int N, const void *alpha, void *X,
        const int incX);
    void flexiblas_real_cblas_zdscal( const int N, const double alpha, void *X,
                       const int incX);
    void flexiblas_chain_cblas_zdscal( const int N, const double alpha, void *X,
                       const int incX);
    void flexiblas_real_cblas_zswap( const int N, void *X, const int incX, void *Y,
                       const int incY);
    void flexiblas_chain_cblas_zswap( const int N, void *X, const int incX, void *Y,
                       const int incY);
    void flexiblas_real_cblas_zsymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        const void  *B, const int ldb, const void *beta,
        void  *C, const int ldc);
    void flexiblas_chain_cblas_zsymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        const void  *B, const int ldb, const void *beta,
        void  *C, const int ldc);
    void flexiblas_real_cblas_zsyr2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void  *B, const int ldb, const void *beta,
        void  *C, const int ldc);
    void flexiblas_chain_cblas_zsyr2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void  *B, const int ldb, const void *beta,
        void  *C, const int ldc);
    void flexiblas_real_cblas_zsyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void *beta, void  *C, const int ldc);
    void flexiblas_chain_cblas_zsyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void *beta, void  *C, const int ldc);
    void flexiblas_real_cblas_zsyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void *beta, void  *C, const int ldc);
    void flexiblas_chain_cblas_zsyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const void *alpha, const void  *A, const int lda,
        const void *beta, void  *C, const int ldc);
    void flexiblas_real_cblas_ztbmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_chain_cblas_ztbmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_real_cblas_ztbsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_chain_cblas_ztbsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_chain_cblas_ztpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *Ap, void  *X, const int incX);
    void flexiblas_real_cblas_ztpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *Ap, void  *X, const int incX);
    void flexiblas_chain_cblas_ztpsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *Ap, void  *X, const int incX);
    void flexiblas_real_cblas_ztpsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *Ap, void  *X, const int incX);
    void flexiblas_chain_cblas_ztrmm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        void  *B, const int ldb);
    void flexiblas_real_cblas_ztrmm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        void  *B, const int ldb);
    void flexiblas_real_cblas_ztrmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_chain_cblas_ztrmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *A, const int lda,
        void  *X, const int incX);
    void flexiblas_chain_cblas_ztrsm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        void  *B, const int ldb);
    void flexiblas_real_cblas_ztrsm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        void  *B, const int ldb);
    void flexiblas_real_cblas_ztrsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *A, const int lda, void  *X,
        const int incX);
    void flexiblas_chain_cblas_ztrsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const void  *A, const int lda, void  *X,
        const int incX);


    /* Double Precision */
    double flexiblas_chain_cblas_dasum( const int N, const double *X, const int incX);
    double flexiblas_real_cblas_dasum( const int N, const double *X, const int incX);
    void flexiblas_real_cblas_daxpy( const int N, const double alpha, const double *X, const int incX, double *Y, const int incY);
    void flexiblas_chain_cblas_daxpy( const int N, const double alpha, const double *X, const int incX, double *Y, const int incY);
    void flexiblas_real_cblas_dcopy( const int N, const double *X,const int incX, double *Y, const int incY);
    void flexiblas_chain_cblas_dcopy( const int N, const double *X,const int incX, double *Y, const int incY);
    double flexiblas_chain_cblas_ddot( const int N, const double *X, const int incX, const double *Y, const int incY);
    double flexiblas_real_cblas_ddot( const int N, const double *X, const int incX, const double *Y, const int incY);
    void flexiblas_chain_cblas_dgbmv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const int KL, const int KU,
        const double alpha, const double  *A, const int lda,
        const double  *X, const int incX, const double beta,
        double  *Y, const int incY);
    void flexiblas_real_cblas_dgbmv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const int KL, const int KU,
        const double alpha, const double  *A, const int lda,
        const double  *X, const int incX, const double beta,
        double  *Y, const int incY);
    void flexiblas_chain_cblas_dgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const int M, const int N,
        const int K, const double alpha, const double  *A,
        const int lda, const double  *B, const int ldb,
        const double beta, double  *C, const int ldc);
    void flexiblas_real_cblas_dgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const int M, const int N,
        const int K, const double alpha, const double  *A,
        const int lda, const double  *B, const int ldb,
        const double beta, double  *C, const int ldc);
    void flexiblas_chain_cblas_dgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const double alpha, const double  *A, const int lda,
        const double  *X, const int incX, const double beta,
        double  *Y, const int incY);
    void flexiblas_real_cblas_dgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const double alpha, const double  *A, const int lda,
        const double  *X, const int incX, const double beta,
        double  *Y, const int incY);
    void flexiblas_real_cblas_dger(const CBLAS_LAYOUT layout, const int M, const int N,
        const double alpha, const double  *X, const int incX,
        const double  *Y, const int incY, double  *A, const int lda);
    void flexiblas_real_cblas_dger(const CBLAS_LAYOUT layout, const int M, const int N,
        const double alpha, const double  *X, const int incX,
        const double  *Y, const int incY, double  *A, const int lda);
    double flexiblas_chain_cblas_dnrm2( const int N, const double *X, const int incX);
    double flexiblas_real_cblas_dnrm2( const int N, const double *X, const int incX);
    void flexiblas_real_cblas_drot(const int N, double *X, const int incX, double *Y, const int incY, const double c, const double s);
    void flexiblas_chain_cblas_drot(const int N, double *X, const int incX, double *Y, const int incY, const double c, const double s);
    void flexiblas_chain_cblas_drotg(  double *a, double *b, double *c, double *s);
    void flexiblas_real_cblas_drotg(  double *a, double *b, double *c, double *s);
    void flexiblas_chain_cblas_drotm( const int N, double *X, const int incX, double *Y,
        const int incY, const double *P);
    void flexiblas_real_cblas_drotm( const int N, double *X, const int incX, double *Y,
        const int incY, const double *P);
    void flexiblas_chain_cblas_drotmg( double *d1, double *d2, double *b1,
        const double b2, double *p);
    void flexiblas_real_cblas_drotmg( double *d1, double *d2, double *b1,
        const double b2, double *p);
    void flexiblas_chain_cblas_dsbmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N, const int K,
        const double alpha, const double  *A, const int lda,
        const double  *X, const int incX, const double beta,
        double  *Y, const int incY);
    void flexiblas_real_cblas_dsbmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N, const int K,
        const double alpha, const double  *A, const int lda,
        const double  *X, const int incX, const double beta,
        double  *Y, const int incY);
    void flexiblas_chain_cblas_dscal( const int N, const double alpha, double *X, const int incX);
    void flexiblas_real_cblas_dscal( const int N, const double alpha, double *X, const int incX);
    double flexiblas_real_cblas_dsdot( const int N, const float *X, const int incX, const float *Y, const int incY);
    double flexiblas_chain_cblas_dsdot( const int N, const float *X, const int incX, const float *Y, const int incY);
    void flexiblas_chain_cblas_dspmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const double alpha, const double  *AP,
        const double  *X, const int incX, const double beta,
        double  *Y, const int incY);
    void flexiblas_real_cblas_dspmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const double alpha, const double  *AP,
        const double  *X, const int incX, const double beta,
        double  *Y, const int incY);
    void flexiblas_chain_cblas_dspr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const double alpha, const double *X,
        const int incX, double *Ap);
    void flexiblas_real_cblas_dspr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const double alpha, const double *X,
        const int incX, double *Ap);
    void flexiblas_real_cblas_dspr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const double  alpha, const double  *X,
                const int incX, const double  *Y, const int incY, double  *A);
    void flexiblas_chain_cblas_dspr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const double  alpha, const double  *X,
                const int incX, const double  *Y, const int incY, double  *A);
    void flexiblas_chain_cblas_dswap( const int N, double *X, const int incX, double *Y,
        const int incY);
    void flexiblas_real_cblas_dswap( const int N, double *X, const int incX, double *Y,
        const int incY);
    void flexiblas_real_cblas_dsymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const int M, const int N,
                 const double alpha, const double  *A, const int lda,
                 const double  *B, const int ldb, const double beta,
                 double  *C, const int ldc);
    void flexiblas_chain_cblas_dsymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const int M, const int N,
                 const double alpha, const double  *A, const int lda,
                 const double  *B, const int ldb, const double beta,
                 double  *C, const int ldc);
    void flexiblas_chain_cblas_dsymv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const double alpha, const double  *A, const int lda,
        const double  *X, const int incX, const double beta,
        double  *Y, const int incY);
    void flexiblas_real_cblas_dsymv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const double alpha, const double  *A, const int lda,
        const double  *X, const int incX, const double beta,
        double  *Y, const int incY);
    void flexiblas_chain_cblas_dsyr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const double  alpha, const double  *X,
        const int incX, double  *A, const int lda);
    void flexiblas_real_cblas_dsyr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const double  alpha, const double  *X,
        const int incX, double  *A, const int lda);
    void flexiblas_chain_cblas_dsyr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const double  alpha, const double  *X,
                const int incX, const double  *Y, const int incY, double  *A,
                const int lda);
    void flexiblas_real_cblas_dsyr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const double  alpha, const double  *X,
                const int incX, const double  *Y, const int incY, double  *A,
                const int lda);
    void flexiblas_real_cblas_dsyr2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const double alpha, const double  *A, const int lda,
        const double  *B, const int ldb, const double beta,
        double  *C, const int ldc);
    void flexiblas_chain_cblas_dsyr2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const double alpha, const double  *A, const int lda,
        const double  *B, const int ldb, const double beta,
        double  *C, const int ldc);
    void flexiblas_real_cblas_dsyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const double alpha, const double  *A, const int lda,
        const double beta, double  *C, const int ldc);
    void flexiblas_chain_cblas_dsyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const double alpha, const double  *A, const int lda,
        const double beta, double  *C, const int ldc);
    void flexiblas_chain_cblas_dtbmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const double  *A, const int lda,
        double  *X, const int incX);
    void flexiblas_real_cblas_dtbmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const double  *A, const int lda,
        double  *X, const int incX);
    void flexiblas_chain_cblas_dtbsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const double  *A, const int lda,
        double  *X, const int incX);
    void flexiblas_real_cblas_dtbsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const double  *A, const int lda,
        double  *X, const int incX);
    void flexiblas_real_cblas_dtpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const double  *Ap, double  *X, const int incX);
    void flexiblas_chain_cblas_dtpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const double  *Ap, double  *X, const int incX);
    void flexiblas_chain_cblas_dtpsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const double  *Ap, double  *X, const int incX);
    void flexiblas_real_cblas_dtpsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const double  *Ap, double  *X, const int incX);
    void flexiblas_chain_cblas_dtrmm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const double alpha, const double  *A, const int lda,
        double  *B, const int ldb);
    void flexiblas_real_cblas_dtrmm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const double alpha, const double  *A, const int lda,
        double  *B, const int ldb);
    void flexiblas_real_cblas_dtrmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const double  *A, const int lda,
        double  *X, const int incX);
    void flexiblas_chain_cblas_dtrmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const double  *A, const int lda,
        double  *X, const int incX);
    void flexiblas_real_cblas_dtrsm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const double alpha, const double  *A, const int lda,
        double  *B, const int ldb);
    void flexiblas_chain_cblas_dtrsm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const double alpha, const double  *A, const int lda,
        double  *B, const int ldb);
    void flexiblas_chain_cblas_dtrsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const int N, const double  *A, const int lda, double  *X,
                 const int incX);
    void flexiblas_real_cblas_dtrsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const int N, const double  *A, const int lda, double  *X,
                 const int incX);
    double flexiblas_chain_cblas_dzasum( const int N, const void *X, const int incX);
    double flexiblas_real_cblas_dzasum( const int N, const void *X, const int incX);
    double flexiblas_chain_cblas_dznrm2( const int N, const void *X, const int incX);
    double flexiblas_real_cblas_dznrm2( const int N, const void *X, const int incX);


    /* Single Precision  */
    float flexiblas_chain_cblas_sdsdot( const int N, const float alpha, const float *X, const int incX, const float *Y, const int incY);
    float flexiblas_real_cblas_sdsdot( const int N, const float alpha, const float *X, const int incX, const float *Y, const int incY);
    float flexiblas_chain_cblas_scasum( const int N, const void *X, const int incX);
    float flexiblas_real_cblas_scasum( const int N, const void *X, const int incX);
    float flexiblas_chain_cblas_scnrm2( const int N, const void *X, const int incX);
    float flexiblas_real_cblas_scnrm2( const int N, const void *X, const int incX);

    float flexiblas_chain_cblas_sasum( const int N, const float *X, const int incX);
    float flexiblas_real_cblas_sasum( const int N, const float *X, const int incX);
    void flexiblas_real_cblas_saxpy( const int N, const float alpha, const float *X, const int incX, float *Y, const int incY);
    void flexiblas_chain_cblas_saxpy( const int N, const float alpha, const float *X, const int incX, float *Y, const int incY);
    void flexiblas_real_cblas_scopy( const int N, const float *X,const int incX, float *Y, const int incY);
    void flexiblas_chain_cblas_scopy( const int N, const float *X,const int incX, float *Y, const int incY);
    float flexiblas_chain_cblas_sdot( const int N, const float *X, const int incX, const float *Y, const int incY);
    float flexiblas_real_cblas_sdot( const int N, const float *X, const int incX, const float *Y, const int incY);
    void flexiblas_chain_cblas_sgbmv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const int KL, const int KU,
        const float alpha, const float  *A, const int lda,
        const float  *X, const int incX, const float beta,
        float  *Y, const int incY);
    void flexiblas_real_cblas_sgbmv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const int KL, const int KU,
        const float alpha, const float  *A, const int lda,
        const float  *X, const int incX, const float beta,
        float  *Y, const int incY);
    void flexiblas_chain_cblas_sgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const int M, const int N,
        const int K, const float alpha, const float  *A,
        const int lda, const float  *B, const int ldb,
        const float beta, float  *C, const int ldc);
    void flexiblas_real_cblas_sgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const int M, const int N,
        const int K, const float alpha, const float  *A,
        const int lda, const float  *B, const int ldb,
        const float beta, float  *C, const int ldc);
    void flexiblas_chain_cblas_sgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const float alpha, const float  *A, const int lda,
        const float  *X, const int incX, const float beta,
        float  *Y, const int incY);
    void flexiblas_real_cblas_sgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const float alpha, const float  *A, const int lda,
        const float  *X, const int incX, const float beta,
        float  *Y, const int incY);
    void flexiblas_real_cblas_sger(const CBLAS_LAYOUT layout, const int M, const int N,
        const float alpha, const float  *X, const int incX,
        const float  *Y, const int incY, float  *A, const int lda);
    void flexiblas_real_cblas_sger(const CBLAS_LAYOUT layout, const int M, const int N,
        const float alpha, const float  *X, const int incX,
        const float  *Y, const int incY, float  *A, const int lda);
    float flexiblas_chain_cblas_snrm2( const int N, const float *X, const int incX);
    float flexiblas_real_cblas_snrm2( const int N, const float *X, const int incX);
    void flexiblas_real_cblas_srot(const int N, float *X, const int incX, float *Y, const int incY, const float c, const float s);
    void flexiblas_chain_cblas_srot(const int N, float *X, const int incX, float *Y, const int incY, const float c, const float s);
    void flexiblas_chain_cblas_srotg(  float *a, float *b, float *c, float *s);
    void flexiblas_real_cblas_srotg(  float *a, float *b, float *c, float *s);
    void flexiblas_chain_cblas_srotm( const int N, float *X, const int incX, float *Y,
        const int incY, const float *P);
    void flexiblas_real_cblas_srotm( const int N, float *X, const int incX, float *Y,
        const int incY, const float *P);
    void flexiblas_chain_cblas_srotmg( float *d1, float *d2, float *b1,
        const float b2, float *p);
    void flexiblas_real_cblas_srotmg( float *d1, float *d2, float *b1,
        const float b2, float *p);
    void flexiblas_chain_cblas_ssbmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N, const int K,
        const float alpha, const float  *A, const int lda,
        const float  *X, const int incX, const float beta,
        float  *Y, const int incY);
    void flexiblas_real_cblas_ssbmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N, const int K,
        const float alpha, const float  *A, const int lda,
        const float  *X, const int incX, const float beta,
        float  *Y, const int incY);
    void flexiblas_chain_cblas_sscal( const int N, const float alpha, float *X, const int incX);
    void flexiblas_real_cblas_sscal( const int N, const float alpha, float *X, const int incX);
    void flexiblas_chain_cblas_sspmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const float alpha, const float  *AP,
        const float  *X, const int incX, const float beta,
        float  *Y, const int incY);
    void flexiblas_real_cblas_sspmv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const float alpha, const float  *AP,
        const float  *X, const int incX, const float beta,
        float  *Y, const int incY);
    void flexiblas_chain_cblas_sspr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const float alpha, const float *X,
        const int incX, float *Ap);
    void flexiblas_real_cblas_sspr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const float alpha, const float *X,
        const int incX, float *Ap);
    void flexiblas_real_cblas_sspr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const float  alpha, const float  *X,
                const int incX, const float  *Y, const int incY, float  *A);
    void flexiblas_chain_cblas_sspr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const float  alpha, const float  *X,
                const int incX, const float  *Y, const int incY, float  *A);
    void flexiblas_chain_cblas_sswap( const int N, float *X, const int incX, float *Y,
        const int incY);
    void flexiblas_real_cblas_sswap( const int N, float *X, const int incX, float *Y,
        const int incY);
    void flexiblas_real_cblas_ssymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const int M, const int N,
                 const float alpha, const float  *A, const int lda,
                 const float  *B, const int ldb, const float beta,
                 float  *C, const int ldc);
    void flexiblas_chain_cblas_ssymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const int M, const int N,
                 const float alpha, const float  *A, const int lda,
                 const float  *B, const int ldb, const float beta,
                 float  *C, const int ldc);
    void flexiblas_chain_cblas_ssymv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const float alpha, const float  *A, const int lda,
        const float  *X, const int incX, const float beta,
        float  *Y, const int incY);
    void flexiblas_real_cblas_ssymv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const float alpha, const float  *A, const int lda,
        const float  *X, const int incX, const float beta,
        float  *Y, const int incY);
    void flexiblas_chain_cblas_ssyr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const float  alpha, const float  *X,
        const int incX, float  *A, const int lda);
    void flexiblas_real_cblas_ssyr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const float  alpha, const float  *X,
        const int incX, float  *A, const int lda);
    void flexiblas_chain_cblas_ssyr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const float  alpha, const float  *X,
                const int incX, const float  *Y, const int incY, float  *A,
                const int lda);
    void flexiblas_real_cblas_ssyr2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const float  alpha, const float  *X,
                const int incX, const float  *Y, const int incY, float  *A,
                const int lda);
    void flexiblas_real_cblas_ssyr2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const float alpha, const float  *A, const int lda,
        const float  *B, const int ldb, const float beta,
        float  *C, const int ldc);
    void flexiblas_chain_cblas_ssyr2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const float alpha, const float  *A, const int lda,
        const float  *B, const int ldb, const float beta,
        float  *C, const int ldc);
    void flexiblas_real_cblas_ssyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const float alpha, const float  *A, const int lda,
        const float beta, float  *C, const int ldc);
    void flexiblas_chain_cblas_ssyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const int N, const int K,
        const float alpha, const float  *A, const int lda,
        const float beta, float  *C, const int ldc);
    void flexiblas_chain_cblas_stbmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const float  *A, const int lda,
        float  *X, const int incX);
    void flexiblas_real_cblas_stbmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const float  *A, const int lda,
        float  *X, const int incX);
    void flexiblas_chain_cblas_stbsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const float  *A, const int lda,
        float  *X, const int incX);
    void flexiblas_real_cblas_stbsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const int K, const float  *A, const int lda,
        float  *X, const int incX);
    void flexiblas_real_cblas_stpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const float  *Ap, float  *X, const int incX);
    void flexiblas_chain_cblas_stpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const float  *Ap, float  *X, const int incX);
    void flexiblas_chain_cblas_stpsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const float  *Ap, float  *X, const int incX);
    void flexiblas_real_cblas_stpsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const float  *Ap, float  *X, const int incX);
    void flexiblas_chain_cblas_strmm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const float alpha, const float  *A, const int lda,
        float  *B, const int ldb);
    void flexiblas_real_cblas_strmm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const float alpha, const float  *A, const int lda,
        float  *B, const int ldb);
    void flexiblas_real_cblas_strmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const float  *A, const int lda,
        float  *X, const int incX);
    void flexiblas_chain_cblas_strmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const int N, const float  *A, const int lda,
        float  *X, const int incX);
    void flexiblas_real_cblas_strsm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const float alpha, const float  *A, const int lda,
        float  *B, const int ldb);
    void flexiblas_chain_cblas_strsm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const float alpha, const float  *A, const int lda,
        float  *B, const int ldb);
    void flexiblas_chain_cblas_strsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const int N, const float  *A, const int lda, float  *X,
                 const int incX);
    void flexiblas_real_cblas_strsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const int N, const float  *A, const int lda, float  *X,
                 const int incX);


    /* Integer Functions */
    CBLAS_INDEX flexiblas_chain_cblas_icamax( const int N, const void *X, const int incX);
    CBLAS_INDEX flexiblas_real_cblas_icamax( const int N, const void *X, const int incX);
    CBLAS_INDEX flexiblas_chain_cblas_izamax( const int N, const void *X, const int incX);
    CBLAS_INDEX flexiblas_real_cblas_izamax( const int N, const void *X, const int incX);
    CBLAS_INDEX flexiblas_chain_cblas_idamax( const int N, const double *X, const int incX);
    CBLAS_INDEX flexiblas_real_cblas_idamax( const int N, const double *X, const int incX);
    CBLAS_INDEX flexiblas_chain_cblas_isamax( const int N, const float *X, const int incX);
    CBLAS_INDEX flexiblas_real_cblas_isamax( const int N, const float *X, const int incX);

    void flexiblas_chain_cblas_caxpby( const int N, const void *alpha, const void *X,
                       const int incX, const void *beta, void *Y, const int incY);
    void flexiblas_real_cblas_caxpby( const int N, const void *alpha, const void *X,
                       const int incX, const void *beta, void *Y, const int incY);
    void flexiblas_chain_cblas_zaxpby( const int N, const void *alpha, const void *X,
                       const int incX, const void *beta, void *Y, const int incY);
    void flexiblas_real_cblas_zaxpby( const int N, const void *alpha, const void *X,
                       const int incX, const void *beta, void *Y, const int incY);
    void flexiblas_chain_cblas_daxpby( const int N, const double alpha, const double *X,
                       const int incX, const double beta, double *Y, const int incY);
    void flexiblas_real_cblas_daxpby( const int N, const double alpha, const double *X,
                       const int incX, const double beta, double *Y, const int incY);
    void flexiblas_chain_cblas_saxpby( const int N, const float alpha, const float *X,
                       const int incX, const float beta, float *Y, const int incY);
    void flexiblas_real_cblas_saxpby( const int N, const float alpha, const float *X,
                       const int incX, const float beta, float *Y, const int incY);
    void flexiblas_chain_cblas_cimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const int crows, const int ccols, const void* calpha, void* a, const int clda,
        const int cldb);
    void flexiblas_real_cblas_cimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const int crows, const int ccols, const void* calpha, void* a, const int clda,
        const int cldb);
    void flexiblas_chain_cblas_zimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const int crows, const int ccols, const void* calpha, void* a, const int clda,
        const int cldb);
    void flexiblas_real_cblas_zimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const int crows, const int ccols, const void* calpha, void* a, const int clda,
        const int cldb);
    void flexiblas_chain_cblas_simatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
		     const int crows, const int ccols, const float calpha, float *a, const int clda,
		     const int cldb);
    void flexiblas_real_cblas_simatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
		     const int crows, const int ccols, const float calpha, float *a, const int clda,
		     const int cldb);
    void flexiblas_chain_cblas_dimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
		     const int crows, const int ccols, const double calpha, double *a, const int clda,
		     const int cldb);
    void flexiblas_real_cblas_dimatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
		     const int crows, const int ccols, const double calpha, double *a, const int clda,
		     const int cldb);
    void flexiblas_chain_cblas_domatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const int crows, const int ccols, const double calpha, const double *a, const int clda,
        double *b, const int cldb);
    void flexiblas_real_cblas_domatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const int crows, const int ccols, const double calpha, const double *a, const int clda,
        double *b, const int cldb);
    void flexiblas_chain_cblas_somatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const int crows, const int ccols, const float calpha, const float *a, const int clda,
        float *b, const int cldb);
    void flexiblas_real_cblas_somatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const int crows, const int ccols, const float calpha, const float *a, const int clda,
        float *b, const int cldb);
    void flexiblas_chain_cblas_comatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
             const int crows, const int ccols, const void* calpha, const void* a, const int clda,
             void *b, const int cldb);
    void flexiblas_real_cblas_comatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
             const int crows, const int ccols, const void* calpha, const void* a, const int clda,
             void *b, const int cldb);
    void flexiblas_chain_cblas_zomatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
             const int crows, const int ccols, const void* calpha, const void* a, const int clda,
             void *b, const int cldb);
    void flexiblas_real_cblas_zomatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
             const int crows, const int ccols, const void* calpha, const void* a, const int clda,
             void *b, const int cldb);
    void flexiblas_chain_cblas_cgeadd(const CBLAS_ORDER CORDER,
        const int crows, const int ccols, const void *alpha, void *ca, const int clda,
        const void *beta, void *cb, const int cldb);
    void flexiblas_real_cblas_cgeadd(const CBLAS_ORDER CORDER,
        const int crows, const int ccols, const void *alpha, void *ca, const int clda,
        const void *beta, void *cb, const int cldb);
    void flexiblas_chain_cblas_zgeadd(const CBLAS_ORDER CORDER,
        const int crows, const int ccols, const void *alpha, void *ca, const int clda,
        const void *beta, void *cb, const int cldb);
    void flexiblas_real_cblas_zgeadd(const CBLAS_ORDER CORDER,
        const int crows, const int ccols, const void *alpha, void *ca, const int clda,
        const void *beta, void *cb, const int cldb);
    void flexiblas_chain_cblas_dgeadd(const CBLAS_ORDER CORDER,
        const int crows, const int ccols, const double calpha, double *a, const int clda,
        const double cbeta, double *b, const int cldb);
    void flexiblas_real_cblas_dgeadd(const CBLAS_ORDER CORDER,
        const int crows, const int ccols, const double calpha, double *a, const int clda,
        const double cbeta, double *b, const int cldb);
    void flexiblas_chain_cblas_sgeadd(const CBLAS_ORDER CORDER,
        const int crows, const int ccols, const float calpha, float *a, const int clda,
        const float cbeta, float *b, const int cldb);
    void flexiblas_real_cblas_sgeadd(const CBLAS_ORDER CORDER,
        const int crows, const int ccols, const float calpha, float *a, const int clda,
        const float cbeta, float *b, const int cldb);





#ifdef __cplusplus
}
#endif
#endif
