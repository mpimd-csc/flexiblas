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

#ifndef FLEXIBLAS_REAL_LAPACKE_CALLS_H
#define FLEXIBLAS_REAL_LAPACKE_CALLS_H

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

lapack_int flexiblas_real_LAPACKE_cbbcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e);
lapack_int flexiblas_chain_LAPACKE_cbbcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e);

lapack_int flexiblas_real_LAPACKE_cbbcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e, void *rwork, lapack_int lrwork);
lapack_int flexiblas_chain_LAPACKE_cbbcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e, void *rwork, lapack_int lrwork);

lapack_int flexiblas_real_LAPACKE_cbdsqr(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cbdsqr(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cbdsqr_work(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_cbdsqr_work(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_cgbbrd(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cgbbrd(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cgbbrd_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgbbrd_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgbcon(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_cgbcon(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_cgbcon_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, float anorm, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgbcon_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, float anorm, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgbequ(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_cgbequ(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_cgbequ_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_cgbequ_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_cgbequb(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_cgbequb(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_cgbequb_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_cgbequb_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_cgbrfs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cgbrfs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cgbrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgbrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgbsv(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgbsv(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgbsv_work(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgbsv_work(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgbsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);
lapack_int flexiblas_chain_LAPACKE_cgbsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);

lapack_int flexiblas_real_LAPACKE_cgbsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgbsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgbtrf(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_cgbtrf(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);

lapack_int flexiblas_real_LAPACKE_cgbtrf_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_cgbtrf_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);

lapack_int flexiblas_real_LAPACKE_cgbtrs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgbtrs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgbtrs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgbtrs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgebak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_cgebak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_cgebak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_cgebak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_cgebal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);
lapack_int flexiblas_chain_LAPACKE_cgebal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);

lapack_int flexiblas_real_LAPACKE_cgebal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);
lapack_int flexiblas_chain_LAPACKE_cgebal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);

lapack_int flexiblas_real_LAPACKE_cgebrd(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup);
lapack_int flexiblas_chain_LAPACKE_cgebrd(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup);

lapack_int flexiblas_real_LAPACKE_cgebrd_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgebrd_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgecon(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_cgecon(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_cgecon_work(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgecon_work(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgedmd_work(int matrix_layout, char jobs, char jobz, char jobr, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *eigs, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *w, lapack_int ldw, void *s, lapack_int lds, void *zwork, lapack_int lzwork, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_cgedmd_work(int matrix_layout, char jobs, char jobz, char jobr, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *eigs, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *w, lapack_int ldw, void *s, lapack_int lds, void *zwork, lapack_int lzwork, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_cgedmdq_work(int matrix_layout, char jobs, char jobz, char jobr, char jobq, char jobt, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *f, lapack_int ldf, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *eigs, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *v, lapack_int ldv, void *s, lapack_int lds, void *zwork, lapack_int lzwork, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_cgedmdq_work(int matrix_layout, char jobs, char jobz, char jobr, char jobq, char jobt, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *f, lapack_int ldf, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *eigs, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *v, lapack_int ldv, void *s, lapack_int lds, void *zwork, lapack_int lzwork, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_cgeequ(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_cgeequ(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_cgeequ_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_cgeequ_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_cgeequb(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_cgeequb(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_cgeequb_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_cgeequb_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_cgees(int matrix_layout, char jobvs, char sort, LAPACK_C_SELECT1 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs);
lapack_int flexiblas_chain_LAPACKE_cgees(int matrix_layout, char jobvs, char sort, LAPACK_C_SELECT1 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs);

lapack_int flexiblas_real_LAPACKE_cgees_work(int matrix_layout, char jobvs, char sort, LAPACK_C_SELECT1 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *work, lapack_int lwork, void *rwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_cgees_work(int matrix_layout, char jobvs, char sort, LAPACK_C_SELECT1 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *work, lapack_int lwork, void *rwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_cgeesx(int matrix_layout, char jobvs, char sort, LAPACK_C_SELECT1 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_cgeesx(int matrix_layout, char jobvs, char sort, LAPACK_C_SELECT1 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_cgeesx_work(int matrix_layout, char jobvs, char sort, LAPACK_C_SELECT1 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_cgeesx_work(int matrix_layout, char jobvs, char sort, LAPACK_C_SELECT1 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_cgeev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_cgeev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_cgeev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgeev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgeevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_cgeevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_cgeevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgeevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgehrd(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_cgehrd(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_cgehrd_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgehrd_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgejsv(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *stat, void *istat);
lapack_int flexiblas_chain_LAPACKE_cgejsv(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *stat, void *istat);

lapack_int flexiblas_real_LAPACKE_cgejsv_work(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *cwork, lapack_int lwork, void *work, lapack_int lrwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_cgejsv_work(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *cwork, lapack_int lwork, void *work, lapack_int lrwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_cgelq(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);
lapack_int flexiblas_chain_LAPACKE_cgelq(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);

lapack_int flexiblas_real_LAPACKE_cgelq2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_cgelq2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_cgelq2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_cgelq2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_cgelq_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgelq_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgelqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_cgelqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_cgelqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgelqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgels(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgels(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgels_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgels_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgelsd(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_cgelsd(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_cgelsd_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank, void *work, lapack_int lwork, void *rwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_cgelsd_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank, void *work, lapack_int lwork, void *rwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_cgelss(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_cgelss(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_cgelss_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgelss_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgelsy(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, float rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_cgelsy(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, float rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_cgelsy_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, float rcond, void *rank, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgelsy_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, float rcond, void *rank, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgemlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cgemlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cgemlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgemlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgemqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cgemqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cgemqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgemqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgemqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cgemqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cgemqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_cgemqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_cgeqlf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_cgeqlf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_cgeqlf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgeqlf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgeqp3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);
lapack_int flexiblas_chain_LAPACKE_cgeqp3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);

lapack_int flexiblas_real_LAPACKE_cgeqp3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgeqp3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgeqpf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);
lapack_int flexiblas_chain_LAPACKE_cgeqpf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);

lapack_int flexiblas_real_LAPACKE_cgeqpf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgeqpf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgeqr(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);
lapack_int flexiblas_chain_LAPACKE_cgeqr(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);

lapack_int flexiblas_real_LAPACKE_cgeqr2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_cgeqr2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_cgeqr2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_cgeqr2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_cgeqr_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgeqr_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgeqrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_cgeqrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_cgeqrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgeqrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgeqrfp(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_cgeqrfp(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_cgeqrfp_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgeqrfp_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgeqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_cgeqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_cgeqrt2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_cgeqrt2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_cgeqrt2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_cgeqrt2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_cgeqrt3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_cgeqrt3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_cgeqrt3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_cgeqrt3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_cgeqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *work);
lapack_int flexiblas_chain_LAPACKE_cgeqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *work);

lapack_int flexiblas_real_LAPACKE_cgerfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cgerfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cgerfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgerfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgerqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_cgerqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_cgerqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgerqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgesdd(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt);
lapack_int flexiblas_chain_LAPACKE_cgesdd(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt);

lapack_int flexiblas_real_LAPACKE_cgesdd_work(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_cgesdd_work(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_cgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgesvd(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);
lapack_int flexiblas_chain_LAPACKE_cgesvd(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);

lapack_int flexiblas_real_LAPACKE_cgesvd_work(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgesvd_work(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgesvdq(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank);
lapack_int flexiblas_chain_LAPACKE_cgesvdq(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank);

lapack_int flexiblas_real_LAPACKE_cgesvdq_work(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank, void *iwork, lapack_int liwork, void *cwork, lapack_int lcwork, void *rwork, lapack_int lrwork);
lapack_int flexiblas_chain_LAPACKE_cgesvdq_work(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank, void *iwork, lapack_int liwork, void *cwork, lapack_int lcwork, void *rwork, lapack_int lrwork);

lapack_int flexiblas_real_LAPACKE_cgesvdx(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);
lapack_int flexiblas_chain_LAPACKE_cgesvdx(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);

lapack_int flexiblas_real_LAPACKE_cgesvdx_work(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_cgesvdx_work(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_cgesvj(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *stat);
lapack_int flexiblas_chain_LAPACKE_cgesvj(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *stat);

lapack_int flexiblas_real_LAPACKE_cgesvj_work(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *cwork, lapack_int lwork, void *rwork, lapack_int lrwork);
lapack_int flexiblas_chain_LAPACKE_cgesvj_work(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *cwork, lapack_int lwork, void *rwork, lapack_int lrwork);

lapack_int flexiblas_real_LAPACKE_cgesvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);
lapack_int flexiblas_chain_LAPACKE_cgesvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);

lapack_int flexiblas_real_LAPACKE_cgesvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgesvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgetf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_cgetf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_cgetf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_cgetf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_cgetrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_cgetrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_cgetrf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_cgetrf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_cgetrf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_cgetrf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_cgetrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_cgetrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_cgetri(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_cgetri(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_cgetri_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgetri_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgetrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgetrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgetrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgetrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgetsls(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgetsls(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgetsls_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgetsls_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgetsqrhrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_cgetsqrhrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_cgetsqrhrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgetsqrhrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cggbak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_cggbak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_cggbak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_cggbak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_cggbal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale);
lapack_int flexiblas_chain_LAPACKE_cggbal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale);

lapack_int flexiblas_real_LAPACKE_cggbal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale, void *work);
lapack_int flexiblas_chain_LAPACKE_cggbal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale, void *work);

lapack_int flexiblas_real_LAPACKE_cgges(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);
lapack_int flexiblas_chain_LAPACKE_cgges(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);

lapack_int flexiblas_real_LAPACKE_cgges3(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);
lapack_int flexiblas_chain_LAPACKE_cgges3(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);

lapack_int flexiblas_real_LAPACKE_cgges3_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *rwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_cgges3_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *rwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_cgges_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *rwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_cgges_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *rwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_cggesx(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_cggesx(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_cggesx_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *iwork, lapack_int liwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_cggesx_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_C_SELECT2 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *iwork, lapack_int liwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_cggev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_cggev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_cggev3(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_cggev3(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_cggev3_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cggev3_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_cggev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cggev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_cggevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_cggevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_cggevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *iwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_cggevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *iwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_cggglm(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y);
lapack_int flexiblas_chain_LAPACKE_cggglm(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y);

lapack_int flexiblas_real_LAPACKE_cggglm_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cggglm_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgghd3(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_cgghd3(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_cgghd3_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgghd3_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cgghrd(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_cgghrd(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_cgghrd_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_cgghrd_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_cgglse(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x);
lapack_int flexiblas_chain_LAPACKE_cgglse(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x);

lapack_int flexiblas_real_LAPACKE_cgglse_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cgglse_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cggqrf(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);
lapack_int flexiblas_chain_LAPACKE_cggqrf(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);

lapack_int flexiblas_real_LAPACKE_cggqrf_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cggqrf_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cggrqf(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);
lapack_int flexiblas_chain_LAPACKE_cggrqf(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);

lapack_int flexiblas_real_LAPACKE_cggrqf_work(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cggrqf_work(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cggsvd(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);
lapack_int flexiblas_chain_LAPACKE_cggsvd(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);

lapack_int flexiblas_real_LAPACKE_cggsvd3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);
lapack_int flexiblas_chain_LAPACKE_cggsvd3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);

lapack_int flexiblas_real_LAPACKE_cggsvd3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, lapack_int lwork, void *rwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_cggsvd3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, lapack_int lwork, void *rwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_cggsvd_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *rwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_cggsvd_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *rwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_cggsvp(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_cggsvp(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_cggsvp3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_cggsvp3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_cggsvp3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *rwork, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cggsvp3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *rwork, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cggsvp_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *rwork, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_cggsvp_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *rwork, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_cgtcon(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_cgtcon(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_cgtcon_work(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, float anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_cgtcon_work(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, float anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_cgtrfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cgtrfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cgtrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgtrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgtsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgtsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgtsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgtsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgtsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cgtsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cgtsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cgtsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cgttrf(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_cgttrf(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);

lapack_int flexiblas_real_LAPACKE_cgttrf_work(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_cgttrf_work(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);

lapack_int flexiblas_real_LAPACKE_cgttrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgttrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cgttrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cgttrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chbev(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chbev(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chbev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chbev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chbev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chbev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_chbev_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chbev_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_chbevd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chbevd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chbevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chbevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chbevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_chbevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_chbevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_chbevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_chbevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chbevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_chbevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chbevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_chbevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chbevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_chbevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chbevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_chbgst(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx);
lapack_int flexiblas_chain_LAPACKE_chbgst(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx);

lapack_int flexiblas_real_LAPACKE_chbgst_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chbgst_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_chbgv(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chbgv(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chbgv_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chbgv_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_chbgvd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chbgvd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chbgvd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_chbgvd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_chbgvx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chbgvx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_chbgvx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chbgvx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_chbtrd(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_chbtrd(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_chbtrd_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *work);
lapack_int flexiblas_chain_LAPACKE_chbtrd_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *work);

lapack_int flexiblas_real_LAPACKE_checon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_checon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_checon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_checon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_checon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_checon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_checon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_checon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_cheequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_cheequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_cheequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);
lapack_int flexiblas_chain_LAPACKE_cheequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);

lapack_int flexiblas_real_LAPACKE_cheev(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_cheev(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_cheev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_cheev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_cheev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cheev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_cheev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cheev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_cheevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_cheevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_cheevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_cheevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_cheevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_cheevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_cheevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_cheevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_cheevr(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_cheevr(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_cheevr_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_cheevr_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_cheevr_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_cheevr_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_cheevr_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_cheevr_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_cheevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_cheevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_cheevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_cheevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_cheevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_cheevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_cheevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_cheevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_chegst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chegst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chegst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chegst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chegv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_chegv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_chegv_2stage(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_chegv_2stage(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_chegv_2stage_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chegv_2stage_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_chegv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chegv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_chegvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_chegvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_chegvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_chegvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_chegvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chegvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_chegvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chegvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_cherfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cherfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cherfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cherfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_chesv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chesv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chesv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chesv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chesv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chesv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chesv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chesv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chesv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chesv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chesv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chesv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chesv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chesv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chesv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chesv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chesvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_chesvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_chesvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chesvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_cheswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_cheswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_cheswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_cheswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_chetrd(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_chetrd(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_chetrd_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chetrd_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chetrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_chetrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_chetrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_chetrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_chetrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);
lapack_int flexiblas_chain_LAPACKE_chetrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);

lapack_int flexiblas_real_LAPACKE_chetrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chetrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chetrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chetrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chetrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_chetrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);

lapack_int flexiblas_real_LAPACKE_chetrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chetrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chetrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_chetrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_chetrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chetrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chetrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chetrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chetri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_chetri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_chetri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_chetri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_chetri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chetri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chetri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_chetri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_chetri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_chetri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_chetri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_chetri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_chetri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chetri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chetri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_chetri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_chetrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chetrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chetrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chetrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chetrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);
lapack_int flexiblas_chain_LAPACKE_chetrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);

lapack_int flexiblas_real_LAPACKE_chetrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chetrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chetrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chetrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chetrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chetrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chetrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chetrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chetrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chetrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chetrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chetrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_chetrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chetrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chetrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chetrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chetrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chetrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chfrk(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, float alpha, const void *a, lapack_int lda, float beta, void *c);
lapack_int flexiblas_chain_LAPACKE_chfrk(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, float alpha, const void *a, lapack_int lda, float beta, void *c);

lapack_int flexiblas_real_LAPACKE_chfrk_work(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, float alpha, const void *a, lapack_int lda, float beta, void *c);
lapack_int flexiblas_chain_LAPACKE_chfrk_work(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, float alpha, const void *a, lapack_int lda, float beta, void *c);

lapack_int flexiblas_real_LAPACKE_chgeqz(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chgeqz(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chgeqz_work(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chgeqz_work(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_chpcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_chpcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_chpcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_chpcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_chpev(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chpev(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chpev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chpev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_chpevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chpevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chpevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_chpevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_chpevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chpevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_chpevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chpevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_chpgst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);
lapack_int flexiblas_chain_LAPACKE_chpgst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);

lapack_int flexiblas_real_LAPACKE_chpgst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);
lapack_int flexiblas_chain_LAPACKE_chpgst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);

lapack_int flexiblas_real_LAPACKE_chpgv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chpgv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chpgv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chpgv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_chpgvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chpgvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chpgvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_chpgvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_chpgvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chpgvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_chpgvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_chpgvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_chprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_chprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_chprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_chpsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chpsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chpsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chpsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chpsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_chpsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_chpsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_chpsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_chptrd(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_chptrd(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_chptrd_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_chptrd_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_chptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_chptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_chptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_chptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_chptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_chptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_chptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_chptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_chptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_chptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_chsein(int matrix_layout, char job, char eigsrc, char initv, const void *select, lapack_int n, const void *h, lapack_int ldh, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *ifaill, void *ifailr);
lapack_int flexiblas_chain_LAPACKE_chsein(int matrix_layout, char job, char eigsrc, char initv, const void *select, lapack_int n, const void *h, lapack_int ldh, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *ifaill, void *ifailr);

lapack_int flexiblas_real_LAPACKE_chsein_work(int matrix_layout, char job, char eigsrc, char initv, const void *select, lapack_int n, const void *h, lapack_int ldh, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork, void *ifaill, void *ifailr);
lapack_int flexiblas_chain_LAPACKE_chsein_work(int matrix_layout, char job, char eigsrc, char initv, const void *select, lapack_int n, const void *h, lapack_int ldh, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork, void *ifaill, void *ifailr);

lapack_int flexiblas_real_LAPACKE_chseqr(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_chseqr(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_chseqr_work(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_chseqr_work(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_clacgv(lapack_int n, void *x, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_clacgv(lapack_int n, void *x, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_clacgv_work(lapack_int n, void *x, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_clacgv_work(lapack_int n, void *x, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_clacn2(lapack_int n, void *v, void *x, void *est, void *kase, void *isave);
lapack_int flexiblas_chain_LAPACKE_clacn2(lapack_int n, void *v, void *x, void *est, void *kase, void *isave);

lapack_int flexiblas_real_LAPACKE_clacn2_work(lapack_int n, void *v, void *x, void *est, void *kase, void *isave);
lapack_int flexiblas_chain_LAPACKE_clacn2_work(lapack_int n, void *v, void *x, void *est, void *kase, void *isave);

lapack_int flexiblas_real_LAPACKE_clacp2(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_clacp2(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_clacp2_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_clacp2_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_clacpy(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_clacpy(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_clacpy_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_clacpy_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_clacrm(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_clacrm(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_clacrm_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_clacrm_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_clag2z(int matrix_layout, lapack_int m, lapack_int n, const void *sa, lapack_int ldsa, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_clag2z(int matrix_layout, lapack_int m, lapack_int n, const void *sa, lapack_int ldsa, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_clag2z_work(int matrix_layout, lapack_int m, lapack_int n, const void *sa, lapack_int ldsa, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_clag2z_work(int matrix_layout, lapack_int m, lapack_int n, const void *sa, lapack_int ldsa, void *a, lapack_int lda);

float flexiblas_real_LAPACKE_clangb(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab);
float flexiblas_chain_LAPACKE_clangb(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab);

float flexiblas_real_LAPACKE_clangb_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *work);
float flexiblas_chain_LAPACKE_clangb_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *work);

float flexiblas_real_LAPACKE_clange(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda);
float flexiblas_chain_LAPACKE_clange(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda);

float flexiblas_real_LAPACKE_clange_work(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);
float flexiblas_chain_LAPACKE_clange_work(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);

float flexiblas_real_LAPACKE_clanhe(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);
float flexiblas_chain_LAPACKE_clanhe(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);

float flexiblas_real_LAPACKE_clanhe_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);
float flexiblas_chain_LAPACKE_clanhe_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);

float flexiblas_real_LAPACKE_clansy(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);
float flexiblas_chain_LAPACKE_clansy(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);

float flexiblas_real_LAPACKE_clansy_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);
float flexiblas_chain_LAPACKE_clansy_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);

float flexiblas_real_LAPACKE_clantr(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda);
float flexiblas_chain_LAPACKE_clantr(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda);

float flexiblas_real_LAPACKE_clantr_work(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);
float flexiblas_chain_LAPACKE_clantr_work(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);

lapack_int flexiblas_real_LAPACKE_clapmr(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_clapmr(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_clapmr_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_clapmr_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_clapmt(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_clapmt(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_clapmt_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_clapmt_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_clarcm(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_clarcm(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_clarcm_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_clarcm_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_clarfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_clarfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_clarfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work, lapack_int ldwork);
lapack_int flexiblas_chain_LAPACKE_clarfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work, lapack_int ldwork);

lapack_int flexiblas_real_LAPACKE_clarfg(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);
lapack_int flexiblas_chain_LAPACKE_clarfg(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);

lapack_int flexiblas_real_LAPACKE_clarfg_work(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);
lapack_int flexiblas_chain_LAPACKE_clarfg_work(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);

lapack_int flexiblas_real_LAPACKE_clarft(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_clarft(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_clarft_work(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_clarft_work(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_clarfx(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, float _Complex tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_clarfx(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, float _Complex tau, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_clarfx_work(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, float _Complex tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_clarfx_work(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, float _Complex tau, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_clarnv(lapack_int idist, void *iseed, lapack_int n, void *x);
lapack_int flexiblas_chain_LAPACKE_clarnv(lapack_int idist, void *iseed, lapack_int n, void *x);

lapack_int flexiblas_real_LAPACKE_clarnv_work(lapack_int idist, void *iseed, lapack_int n, void *x);
lapack_int flexiblas_chain_LAPACKE_clarnv_work(lapack_int idist, void *iseed, lapack_int n, void *x);

lapack_int flexiblas_real_LAPACKE_clascl(int matrix_layout, char type, lapack_int kl, lapack_int ku, float cfrom, float cto, lapack_int m, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_clascl(int matrix_layout, char type, lapack_int kl, lapack_int ku, float cfrom, float cto, lapack_int m, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_clascl_work(int matrix_layout, char type, lapack_int kl, lapack_int ku, float cfrom, float cto, lapack_int m, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_clascl_work(int matrix_layout, char type, lapack_int kl, lapack_int ku, float cfrom, float cto, lapack_int m, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_claset(int matrix_layout, char uplo, lapack_int m, lapack_int n, float _Complex alpha, float _Complex beta, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_claset(int matrix_layout, char uplo, lapack_int m, lapack_int n, float _Complex alpha, float _Complex beta, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_claset_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, float _Complex alpha, float _Complex beta, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_claset_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, float _Complex alpha, float _Complex beta, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_classq(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);
lapack_int flexiblas_chain_LAPACKE_classq(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);

lapack_int flexiblas_real_LAPACKE_classq_work(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);
lapack_int flexiblas_chain_LAPACKE_classq_work(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);

lapack_int flexiblas_real_LAPACKE_claswp(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_claswp(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_claswp_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_claswp_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_clauum(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_clauum(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_clauum_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_clauum_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_cpbcon(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_cpbcon(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_cpbcon_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, float anorm, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cpbcon_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, float anorm, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cpbequ(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_cpbequ(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_cpbequ_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_cpbequ_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_cpbrfs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cpbrfs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cpbrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cpbrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cpbstf(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);
lapack_int flexiblas_chain_LAPACKE_cpbstf(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);

lapack_int flexiblas_real_LAPACKE_cpbstf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);
lapack_int flexiblas_chain_LAPACKE_cpbstf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);

lapack_int flexiblas_real_LAPACKE_cpbsv(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpbsv(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cpbsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpbsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cpbsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cpbsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cpbsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cpbsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cpbtrf(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);
lapack_int flexiblas_chain_LAPACKE_cpbtrf(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);

lapack_int flexiblas_real_LAPACKE_cpbtrf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);
lapack_int flexiblas_chain_LAPACKE_cpbtrf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);

lapack_int flexiblas_real_LAPACKE_cpbtrs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpbtrs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cpbtrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpbtrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cpftrf(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_cpftrf(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_cpftrf_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_cpftrf_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_cpftri(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_cpftri(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_cpftri_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_cpftri_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_cpftrs(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpftrs(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cpftrs_work(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpftrs_work(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cpocon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_cpocon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_cpocon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cpocon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cpoequ(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_cpoequ(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_cpoequ_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_cpoequ_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_cpoequb(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_cpoequb(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_cpoequb_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_cpoequb_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_cporfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cporfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cporfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cporfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cposvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cposvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cposvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cposvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cpotrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_cpotrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_cpotrf2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_cpotrf2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_cpotrf2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_cpotrf2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_cpotrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_cpotrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_cpotri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_cpotri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_cpotri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_cpotri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_cpotrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpotrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cpotrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpotrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cppcon(int matrix_layout, char uplo, lapack_int n, const void *ap, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_cppcon(int matrix_layout, char uplo, lapack_int n, const void *ap, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_cppcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, float anorm, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cppcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, float anorm, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cppequ(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_cppequ(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_cppequ_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_cppequ_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_cpprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cpprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cpprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cpprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cppsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cppsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cppsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cppsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cppsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cppsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cppsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cppsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cpptrf(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_cpptrf(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_cpptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_cpptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_cpptri(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_cpptri(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_cpptri_work(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_cpptri_work(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_cpptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cpptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cpstrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, float tol);
lapack_int flexiblas_chain_LAPACKE_cpstrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, float tol);

lapack_int flexiblas_real_LAPACKE_cpstrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, float tol, void *work);
lapack_int flexiblas_chain_LAPACKE_cpstrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, float tol, void *work);

lapack_int flexiblas_real_LAPACKE_cptcon(lapack_int n, const void *d, const void *e, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_cptcon(lapack_int n, const void *d, const void *e, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_cptcon_work(lapack_int n, const void *d, const void *e, float anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_cptcon_work(lapack_int n, const void *d, const void *e, float anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_cpteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_cpteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_cpteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_cpteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_cptrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cptrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cptrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cptrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cptsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cptsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cptsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cptsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cptsvx(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cptsvx(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cptsvx_work(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cptsvx_work(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cpttrf(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_cpttrf(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_cpttrf_work(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_cpttrf_work(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_cpttrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpttrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cpttrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cpttrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cspcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_cspcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_cspcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_cspcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_csprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_csprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_csprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_csprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_cspsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cspsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cspsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_cspsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cspsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_cspsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_cspsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_cspsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_csptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_csptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_csptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_csptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_csptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_csptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_csptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_csptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_csptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_cstedc(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_cstedc(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_cstedc_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_cstedc_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_cstegr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_cstegr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_cstegr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_cstegr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_cstein(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *ifailv);
lapack_int flexiblas_chain_LAPACKE_cstein(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *ifailv);

lapack_int flexiblas_real_LAPACKE_cstein_work(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *work, void *iwork, void *ifailv);
lapack_int flexiblas_chain_LAPACKE_cstein_work(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *work, void *iwork, void *ifailv);

lapack_int flexiblas_real_LAPACKE_cstemr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac);
lapack_int flexiblas_chain_LAPACKE_cstemr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac);

lapack_int flexiblas_real_LAPACKE_cstemr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_cstemr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_csteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_csteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_csteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_csteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_csycon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_csycon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_csycon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_csycon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_csycon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_csycon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_csycon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_csycon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_csyconv(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);
lapack_int flexiblas_chain_LAPACKE_csyconv(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);

lapack_int flexiblas_real_LAPACKE_csyconv_work(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);
lapack_int flexiblas_chain_LAPACKE_csyconv_work(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);

lapack_int flexiblas_real_LAPACKE_csyequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_csyequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_csyequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);
lapack_int flexiblas_chain_LAPACKE_csyequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);

lapack_int flexiblas_real_LAPACKE_csyr(int matrix_layout, char uplo, lapack_int n, float _Complex alpha, const void *x, lapack_int incx, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_csyr(int matrix_layout, char uplo, lapack_int n, float _Complex alpha, const void *x, lapack_int incx, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_csyr_work(int matrix_layout, char uplo, lapack_int n, float _Complex alpha, const void *x, lapack_int incx, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_csyr_work(int matrix_layout, char uplo, lapack_int n, float _Complex alpha, const void *x, lapack_int incx, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_csyrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_csyrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_csyrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_csyrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_csysv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csysv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csysv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csysv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csysv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csysv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csysv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csysv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csysv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csysv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csysv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csysv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csysv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csysv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csysv_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csysv_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csysv_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csysv_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csysv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csysv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csysvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_csysvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_csysvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_csysvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_csyswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_csyswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_csyswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_csyswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_csytrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_csytrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_csytrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_csytrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_csytrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);
lapack_int flexiblas_chain_LAPACKE_csytrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);

lapack_int flexiblas_real_LAPACKE_csytrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csytrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csytrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csytrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csytrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_csytrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);

lapack_int flexiblas_real_LAPACKE_csytrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csytrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csytrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_csytrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_csytrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csytrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csytrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csytrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csytri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_csytri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_csytri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_csytri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_csytri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csytri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csytri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_csytri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_csytri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_csytri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_csytri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_csytri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_csytri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csytri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csytri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_csytri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_csytrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csytrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csytrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csytrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csytrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);
lapack_int flexiblas_chain_LAPACKE_csytrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);

lapack_int flexiblas_real_LAPACKE_csytrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csytrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csytrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csytrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csytrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csytrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csytrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csytrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csytrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csytrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csytrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_csytrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_csytrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csytrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csytrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csytrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_csytrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_csytrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ctbcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond);
lapack_int flexiblas_chain_LAPACKE_ctbcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond);

lapack_int flexiblas_real_LAPACKE_ctbcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ctbcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ctbrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_ctbrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_ctbrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ctbrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ctbtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ctbtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ctbtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ctbtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ctfsm(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, float _Complex alpha, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ctfsm(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, float _Complex alpha, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ctfsm_work(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, float _Complex alpha, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ctfsm_work(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, float _Complex alpha, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ctftri(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_ctftri(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_ctftri_work(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_ctftri_work(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_ctfttp(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);
lapack_int flexiblas_chain_LAPACKE_ctfttp(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);

lapack_int flexiblas_real_LAPACKE_ctfttp_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);
lapack_int flexiblas_chain_LAPACKE_ctfttp_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);

lapack_int flexiblas_real_LAPACKE_ctfttr(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ctfttr(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ctfttr_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ctfttr_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ctgevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_ctgevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_ctgevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ctgevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ctgexc(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, lapack_int ifst, lapack_int ilst);
lapack_int flexiblas_chain_LAPACKE_ctgexc(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, lapack_int ifst, lapack_int ilst);

lapack_int flexiblas_real_LAPACKE_ctgexc_work(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, lapack_int ifst, lapack_int ilst);
lapack_int flexiblas_chain_LAPACKE_ctgexc_work(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, lapack_int ifst, lapack_int ilst);

lapack_int flexiblas_real_LAPACKE_ctgsen(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif);
lapack_int flexiblas_chain_LAPACKE_ctgsen(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif);

lapack_int flexiblas_real_LAPACKE_ctgsen_work(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_ctgsen_work(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_ctgsja(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *ncycle);
lapack_int flexiblas_chain_LAPACKE_ctgsja(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *ncycle);

lapack_int flexiblas_real_LAPACKE_ctgsja_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *ncycle);
lapack_int flexiblas_chain_LAPACKE_ctgsja_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *ncycle);

lapack_int flexiblas_real_LAPACKE_ctgsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_ctgsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_ctgsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_ctgsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_ctgsyl(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif);
lapack_int flexiblas_chain_LAPACKE_ctgsyl(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif);

lapack_int flexiblas_real_LAPACKE_ctgsyl_work(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_ctgsyl_work(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_ctpcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond);
lapack_int flexiblas_chain_LAPACKE_ctpcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond);

lapack_int flexiblas_real_LAPACKE_ctpcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ctpcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ctpmqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ctpmqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ctpmqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work);
lapack_int flexiblas_chain_LAPACKE_ctpmqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work);

lapack_int flexiblas_real_LAPACKE_ctpqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_ctpqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_ctpqrt2(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_ctpqrt2(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_ctpqrt2_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_ctpqrt2_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_ctpqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt, void *work);
lapack_int flexiblas_chain_LAPACKE_ctpqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt, void *work);

lapack_int flexiblas_real_LAPACKE_ctprfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ctprfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ctprfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int ldwork);
lapack_int flexiblas_chain_LAPACKE_ctprfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int ldwork);

lapack_int flexiblas_real_LAPACKE_ctprfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_ctprfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_ctprfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ctprfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ctptri(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_ctptri(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_ctptri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_ctptri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_ctptrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ctptrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ctptrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ctptrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ctpttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);
lapack_int flexiblas_chain_LAPACKE_ctpttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);

lapack_int flexiblas_real_LAPACKE_ctpttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);
lapack_int flexiblas_chain_LAPACKE_ctpttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);

lapack_int flexiblas_real_LAPACKE_ctpttr(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ctpttr(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ctpttr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ctpttr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ctrcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond);
lapack_int flexiblas_chain_LAPACKE_ctrcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond);

lapack_int flexiblas_real_LAPACKE_ctrcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ctrcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ctrevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_ctrevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_ctrevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ctrevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ctrexc(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, lapack_int ifst, lapack_int ilst);
lapack_int flexiblas_chain_LAPACKE_ctrexc(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, lapack_int ifst, lapack_int ilst);

lapack_int flexiblas_real_LAPACKE_ctrexc_work(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, lapack_int ifst, lapack_int ilst);
lapack_int flexiblas_chain_LAPACKE_ctrexc_work(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, lapack_int ifst, lapack_int ilst);

lapack_int flexiblas_real_LAPACKE_ctrrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_ctrrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_ctrrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ctrrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ctrsen(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *w, void *m, void *s, void *sep);
lapack_int flexiblas_chain_LAPACKE_ctrsen(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *w, void *m, void *s, void *sep);

lapack_int flexiblas_real_LAPACKE_ctrsen_work(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *w, void *m, void *s, void *sep, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ctrsen_work(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *w, void *m, void *s, void *sep, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ctrsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_ctrsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_ctrsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m, void *work, lapack_int ldwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ctrsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m, void *work, lapack_int ldwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_ctrsyl(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);
lapack_int flexiblas_chain_LAPACKE_ctrsyl(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);

lapack_int flexiblas_real_LAPACKE_ctrsyl3_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale, void *swork, lapack_int ldswork);
lapack_int flexiblas_chain_LAPACKE_ctrsyl3_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale, void *swork, lapack_int ldswork);

lapack_int flexiblas_real_LAPACKE_ctrsyl_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);
lapack_int flexiblas_chain_LAPACKE_ctrsyl_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);

lapack_int flexiblas_real_LAPACKE_ctrtri(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ctrtri(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ctrtri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ctrtri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ctrtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ctrtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ctrtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ctrtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ctrttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);
lapack_int flexiblas_chain_LAPACKE_ctrttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);

lapack_int flexiblas_real_LAPACKE_ctrttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);
lapack_int flexiblas_chain_LAPACKE_ctrttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);

lapack_int flexiblas_real_LAPACKE_ctrttp(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);
lapack_int flexiblas_chain_LAPACKE_ctrttp(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);

lapack_int flexiblas_real_LAPACKE_ctrttp_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);
lapack_int flexiblas_chain_LAPACKE_ctrttp_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);

lapack_int flexiblas_real_LAPACKE_ctzrzf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_ctzrzf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_ctzrzf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ctzrzf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cunbdb(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2);
lapack_int flexiblas_chain_LAPACKE_cunbdb(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2);

lapack_int flexiblas_real_LAPACKE_cunbdb_work(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cunbdb_work(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cuncsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t);
lapack_int flexiblas_chain_LAPACKE_cuncsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t);

lapack_int flexiblas_real_LAPACKE_cuncsd2by1(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t);
lapack_int flexiblas_chain_LAPACKE_cuncsd2by1(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t);

lapack_int flexiblas_real_LAPACKE_cuncsd2by1_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_cuncsd2by1_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_cuncsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_cuncsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_cungbr(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_cungbr(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_cungbr_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cungbr_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cunghr(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_cunghr(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_cunghr_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cunghr_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cunglq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_cunglq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_cunglq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cunglq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cungql(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_cungql(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_cungql_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cungql_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cungqr(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_cungqr(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_cungqr_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cungqr_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cungrq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_cungrq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_cungrq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cungrq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cungtr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_cungtr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_cungtr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cungtr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cungtsqr_row(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_cungtsqr_row(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_cungtsqr_row_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cungtsqr_row_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cunhr_col(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);
lapack_int flexiblas_chain_LAPACKE_cunhr_col(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);

lapack_int flexiblas_real_LAPACKE_cunhr_col_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);
lapack_int flexiblas_chain_LAPACKE_cunhr_col_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);

lapack_int flexiblas_real_LAPACKE_cunmbr(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cunmbr(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cunmbr_work(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cunmbr_work(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cunmhr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cunmhr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cunmhr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cunmhr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cunmlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cunmlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cunmlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cunmlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cunmql(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cunmql(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cunmql_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cunmql_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cunmqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cunmqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cunmqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cunmqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cunmrq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cunmrq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cunmrq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cunmrq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cunmrz(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cunmrz(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cunmrz_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cunmrz_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cunmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cunmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cunmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_cunmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_cupgtr(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_cupgtr(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_cupgtr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq, void *work);
lapack_int flexiblas_chain_LAPACKE_cupgtr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq, void *work);

lapack_int flexiblas_real_LAPACKE_cupmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_cupmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_cupmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_cupmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_dbbcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e);
lapack_int flexiblas_chain_LAPACKE_dbbcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e);

lapack_int flexiblas_real_LAPACKE_dbbcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dbbcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dbdsdc(int matrix_layout, char uplo, char compq, lapack_int n, void *d, void *e, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *q, void *iq);
lapack_int flexiblas_chain_LAPACKE_dbdsdc(int matrix_layout, char uplo, char compq, lapack_int n, void *d, void *e, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *q, void *iq);

lapack_int flexiblas_real_LAPACKE_dbdsdc_work(int matrix_layout, char uplo, char compq, lapack_int n, void *d, void *e, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *q, void *iq, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dbdsdc_work(int matrix_layout, char uplo, char compq, lapack_int n, void *d, void *e, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *q, void *iq, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dbdsqr(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dbdsqr(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dbdsqr_work(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_dbdsqr_work(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_dbdsvdx(int matrix_layout, char uplo, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *z, lapack_int ldz, void *superb);
lapack_int flexiblas_chain_LAPACKE_dbdsvdx(int matrix_layout, char uplo, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *z, lapack_int ldz, void *superb);

lapack_int flexiblas_real_LAPACKE_dbdsvdx_work(int matrix_layout, char uplo, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *z, lapack_int ldz, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dbdsvdx_work(int matrix_layout, char uplo, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *z, lapack_int ldz, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_ddisna(char job, lapack_int m, lapack_int n, const void *d, void *sep);
lapack_int flexiblas_chain_LAPACKE_ddisna(char job, lapack_int m, lapack_int n, const void *d, void *sep);

lapack_int flexiblas_real_LAPACKE_ddisna_work(char job, lapack_int m, lapack_int n, const void *d, void *sep);
lapack_int flexiblas_chain_LAPACKE_ddisna_work(char job, lapack_int m, lapack_int n, const void *d, void *sep);

lapack_int flexiblas_real_LAPACKE_dgbbrd(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dgbbrd(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dgbbrd_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_dgbbrd_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_dgbcon(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dgbcon(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_dgbcon_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, double anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgbcon_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, double anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgbequ(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_dgbequ(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_dgbequ_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_dgbequ_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_dgbequb(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_dgbequb(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_dgbequb_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_dgbequb_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_dgbrfs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dgbrfs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dgbrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgbrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgbsv(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgbsv(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgbsv_work(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgbsv_work(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgbsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);
lapack_int flexiblas_chain_LAPACKE_dgbsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);

lapack_int flexiblas_real_LAPACKE_dgbsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgbsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgbtrf(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dgbtrf(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dgbtrf_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dgbtrf_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dgbtrs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgbtrs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgbtrs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgbtrs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgebak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_dgebak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_dgebak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_dgebak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_dgebal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);
lapack_int flexiblas_chain_LAPACKE_dgebal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);

lapack_int flexiblas_real_LAPACKE_dgebal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);
lapack_int flexiblas_chain_LAPACKE_dgebal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);

lapack_int flexiblas_real_LAPACKE_dgebrd(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup);
lapack_int flexiblas_chain_LAPACKE_dgebrd(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup);

lapack_int flexiblas_real_LAPACKE_dgebrd_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgebrd_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgecon(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dgecon(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_dgecon_work(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgecon_work(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgedmd_work(int matrix_layout, char jobs, char jobz, char jobr, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *reig, void *imeig, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *w, lapack_int ldw, void *s, lapack_int lds, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dgedmd_work(int matrix_layout, char jobs, char jobz, char jobr, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *reig, void *imeig, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *w, lapack_int ldw, void *s, lapack_int lds, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dgedmdq_work(int matrix_layout, char jobs, char jobz, char jobr, char jobq, char jobt, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *f, lapack_int ldf, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *reig, void *imeig, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *v, lapack_int ldv, void *s, lapack_int lds, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dgedmdq_work(int matrix_layout, char jobs, char jobz, char jobr, char jobq, char jobt, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *f, lapack_int ldf, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *reig, void *imeig, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *v, lapack_int ldv, void *s, lapack_int lds, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dgeequ(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_dgeequ(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_dgeequ_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_dgeequ_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_dgeequb(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_dgeequb(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_dgeequb_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_dgeequb_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_dgees(int matrix_layout, char jobvs, char sort, LAPACK_D_SELECT2 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs);
lapack_int flexiblas_chain_LAPACKE_dgees(int matrix_layout, char jobvs, char sort, LAPACK_D_SELECT2 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs);

lapack_int flexiblas_real_LAPACKE_dgees_work(int matrix_layout, char jobvs, char sort, LAPACK_D_SELECT2 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *work, lapack_int lwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_dgees_work(int matrix_layout, char jobvs, char sort, LAPACK_D_SELECT2 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *work, lapack_int lwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_dgeesx(int matrix_layout, char jobvs, char sort, LAPACK_D_SELECT2 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_dgeesx(int matrix_layout, char jobvs, char sort, LAPACK_D_SELECT2 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_dgeesx_work(int matrix_layout, char jobvs, char sort, LAPACK_D_SELECT2 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, lapack_int liwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_dgeesx_work(int matrix_layout, char jobvs, char sort, LAPACK_D_SELECT2 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, lapack_int liwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_dgeev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_dgeev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_dgeev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgeev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgeevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_dgeevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_dgeevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgeevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgehrd(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_dgehrd(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_dgehrd_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgehrd_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgejsv(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *stat, void *istat);
lapack_int flexiblas_chain_LAPACKE_dgejsv(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *stat, void *istat);

lapack_int flexiblas_real_LAPACKE_dgejsv_work(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgejsv_work(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgelq(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);
lapack_int flexiblas_chain_LAPACKE_dgelq(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);

lapack_int flexiblas_real_LAPACKE_dgelq2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_dgelq2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_dgelq2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_dgelq2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_dgelq_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgelq_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgelqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_dgelqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_dgelqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgelqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgels(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgels(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgels_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgels_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgelsd(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_dgelsd(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_dgelsd_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgelsd_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgelss(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_dgelss(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_dgelss_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgelss_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgelsy(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, double rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_dgelsy(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, double rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_dgelsy_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, double rcond, void *rank, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgelsy_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, double rcond, void *rank, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgemlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dgemlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dgemlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgemlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgemqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dgemqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dgemqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgemqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgemqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dgemqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dgemqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_dgemqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_dgeqlf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_dgeqlf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_dgeqlf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgeqlf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgeqp3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);
lapack_int flexiblas_chain_LAPACKE_dgeqp3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);

lapack_int flexiblas_real_LAPACKE_dgeqp3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgeqp3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgeqpf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);
lapack_int flexiblas_chain_LAPACKE_dgeqpf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);

lapack_int flexiblas_real_LAPACKE_dgeqpf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_dgeqpf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_dgeqr(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);
lapack_int flexiblas_chain_LAPACKE_dgeqr(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);

lapack_int flexiblas_real_LAPACKE_dgeqr2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_dgeqr2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_dgeqr2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_dgeqr2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_dgeqr_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgeqr_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgeqrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_dgeqrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_dgeqrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgeqrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgeqrfp(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_dgeqrfp(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_dgeqrfp_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgeqrfp_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgeqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dgeqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dgeqrt2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dgeqrt2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dgeqrt2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dgeqrt2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dgeqrt3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dgeqrt3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dgeqrt3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dgeqrt3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dgeqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *work);
lapack_int flexiblas_chain_LAPACKE_dgeqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *work);

lapack_int flexiblas_real_LAPACKE_dgerfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dgerfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dgerfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgerfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgerqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_dgerqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_dgerqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgerqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgesdd(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt);
lapack_int flexiblas_chain_LAPACKE_dgesdd(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt);

lapack_int flexiblas_real_LAPACKE_dgesdd_work(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgesdd_work(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgesvd(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);
lapack_int flexiblas_chain_LAPACKE_dgesvd(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);

lapack_int flexiblas_real_LAPACKE_dgesvd_work(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgesvd_work(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgesvdq(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank);
lapack_int flexiblas_chain_LAPACKE_dgesvdq(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank);

lapack_int flexiblas_real_LAPACKE_dgesvdq_work(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank, void *iwork, lapack_int liwork, void *work, lapack_int lwork, void *rwork, lapack_int lrwork);
lapack_int flexiblas_chain_LAPACKE_dgesvdq_work(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank, void *iwork, lapack_int liwork, void *work, lapack_int lwork, void *rwork, lapack_int lrwork);

lapack_int flexiblas_real_LAPACKE_dgesvdx(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);
lapack_int flexiblas_chain_LAPACKE_dgesvdx(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);

lapack_int flexiblas_real_LAPACKE_dgesvdx_work(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgesvdx_work(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgesvj(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *stat);
lapack_int flexiblas_chain_LAPACKE_dgesvj(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *stat);

lapack_int flexiblas_real_LAPACKE_dgesvj_work(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgesvj_work(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgesvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);
lapack_int flexiblas_chain_LAPACKE_dgesvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);

lapack_int flexiblas_real_LAPACKE_dgesvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgesvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgetf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dgetf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dgetf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dgetf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dgetrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dgetrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dgetrf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dgetrf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dgetrf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dgetrf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dgetrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dgetrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dgetri(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dgetri(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_dgetri_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgetri_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgetrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgetrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgetrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgetrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgetsls(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgetsls(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgetsls_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgetsls_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgetsqrhrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dgetsqrhrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dgetsqrhrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgetsqrhrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dggbak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_dggbak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_dggbak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_dggbak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_dggbal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale);
lapack_int flexiblas_chain_LAPACKE_dggbal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale);

lapack_int flexiblas_real_LAPACKE_dggbal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale, void *work);
lapack_int flexiblas_chain_LAPACKE_dggbal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale, void *work);

lapack_int flexiblas_real_LAPACKE_dgges(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);
lapack_int flexiblas_chain_LAPACKE_dgges(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);

lapack_int flexiblas_real_LAPACKE_dgges3(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);
lapack_int flexiblas_chain_LAPACKE_dgges3(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);

lapack_int flexiblas_real_LAPACKE_dgges3_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_dgges3_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_dgges_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_dgges_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_dggesx(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_dggesx(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_dggesx_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, lapack_int liwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_dggesx_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_D_SELECT3 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, lapack_int liwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_dggev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_dggev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_dggev3(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_dggev3(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_dggev3_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dggev3_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dggev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dggev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dggevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_dggevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_dggevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_dggevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_dggglm(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y);
lapack_int flexiblas_chain_LAPACKE_dggglm(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y);

lapack_int flexiblas_real_LAPACKE_dggglm_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dggglm_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgghd3(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dgghd3(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dgghd3_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgghd3_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dgghrd(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dgghrd(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dgghrd_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dgghrd_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dgglse(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x);
lapack_int flexiblas_chain_LAPACKE_dgglse(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x);

lapack_int flexiblas_real_LAPACKE_dgglse_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dgglse_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dggqrf(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);
lapack_int flexiblas_chain_LAPACKE_dggqrf(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);

lapack_int flexiblas_real_LAPACKE_dggqrf_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dggqrf_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dggrqf(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);
lapack_int flexiblas_chain_LAPACKE_dggrqf(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);

lapack_int flexiblas_real_LAPACKE_dggrqf_work(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dggrqf_work(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dggsvd(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dggsvd(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);

lapack_int flexiblas_real_LAPACKE_dggsvd3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dggsvd3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);

lapack_int flexiblas_real_LAPACKE_dggsvd3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dggsvd3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dggsvd_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dggsvd_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dggsvp(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_dggsvp(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_dggsvp3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_dggsvp3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_dggsvp3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dggsvp3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dggsvp_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_dggsvp_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_dgtcon(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dgtcon(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_dgtcon_work(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, double anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgtcon_work(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, double anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgtrfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dgtrfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dgtrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgtrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgtsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgtsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgtsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgtsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgtsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dgtsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dgtsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dgtsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dgttrf(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dgttrf(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dgttrf_work(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dgttrf_work(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dgttrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgttrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dgttrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dgttrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dhgeqz(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dhgeqz(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dhgeqz_work(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dhgeqz_work(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dhsein(int matrix_layout, char job, char eigsrc, char initv, void *select, lapack_int n, const void *h, lapack_int ldh, void *wr, const void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *ifaill, void *ifailr);
lapack_int flexiblas_chain_LAPACKE_dhsein(int matrix_layout, char job, char eigsrc, char initv, void *select, lapack_int n, const void *h, lapack_int ldh, void *wr, const void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *ifaill, void *ifailr);

lapack_int flexiblas_real_LAPACKE_dhsein_work(int matrix_layout, char job, char eigsrc, char initv, void *select, lapack_int n, const void *h, lapack_int ldh, void *wr, const void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *ifaill, void *ifailr);
lapack_int flexiblas_chain_LAPACKE_dhsein_work(int matrix_layout, char job, char eigsrc, char initv, void *select, lapack_int n, const void *h, lapack_int ldh, void *wr, const void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *ifaill, void *ifailr);

lapack_int flexiblas_real_LAPACKE_dhseqr(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *wr, void *wi, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dhseqr(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *wr, void *wi, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dhseqr_work(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *wr, void *wi, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dhseqr_work(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *wr, void *wi, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dlacn2(lapack_int n, void *v, void *x, void *isgn, void *est, void *kase, void *isave);
lapack_int flexiblas_chain_LAPACKE_dlacn2(lapack_int n, void *v, void *x, void *isgn, void *est, void *kase, void *isave);

lapack_int flexiblas_real_LAPACKE_dlacn2_work(lapack_int n, void *v, void *x, void *isgn, void *est, void *kase, void *isave);
lapack_int flexiblas_chain_LAPACKE_dlacn2_work(lapack_int n, void *v, void *x, void *isgn, void *est, void *kase, void *isave);

lapack_int flexiblas_real_LAPACKE_dlacpy(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dlacpy(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dlacpy_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dlacpy_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dlag2s(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *sa, lapack_int ldsa);
lapack_int flexiblas_chain_LAPACKE_dlag2s(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *sa, lapack_int ldsa);

lapack_int flexiblas_real_LAPACKE_dlag2s_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *sa, lapack_int ldsa);
lapack_int flexiblas_chain_LAPACKE_dlag2s_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *sa, lapack_int ldsa);

double flexiblas_real_LAPACKE_dlamch(char cmach);
double flexiblas_chain_LAPACKE_dlamch(char cmach);

double flexiblas_real_LAPACKE_dlamch_work(char cmach);
double flexiblas_chain_LAPACKE_dlamch_work(char cmach);

double flexiblas_real_LAPACKE_dlangb(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab);
double flexiblas_chain_LAPACKE_dlangb(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab);

double flexiblas_real_LAPACKE_dlangb_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *work);
double flexiblas_chain_LAPACKE_dlangb_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *work);

double flexiblas_real_LAPACKE_dlange(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda);
double flexiblas_chain_LAPACKE_dlange(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda);

double flexiblas_real_LAPACKE_dlange_work(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);
double flexiblas_chain_LAPACKE_dlange_work(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);

double flexiblas_real_LAPACKE_dlansy(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);
double flexiblas_chain_LAPACKE_dlansy(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);

double flexiblas_real_LAPACKE_dlansy_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);
double flexiblas_chain_LAPACKE_dlansy_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);

double flexiblas_real_LAPACKE_dlantr(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda);
double flexiblas_chain_LAPACKE_dlantr(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda);

double flexiblas_real_LAPACKE_dlantr_work(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);
double flexiblas_chain_LAPACKE_dlantr_work(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);

lapack_int flexiblas_real_LAPACKE_dlapmr(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_dlapmr(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_dlapmr_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_dlapmr_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_dlapmt(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_dlapmt(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_dlapmt_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_dlapmt_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

double flexiblas_real_LAPACKE_dlapy2(double x, double y);
double flexiblas_chain_LAPACKE_dlapy2(double x, double y);

double flexiblas_real_LAPACKE_dlapy2_work(double x, double y);
double flexiblas_chain_LAPACKE_dlapy2_work(double x, double y);

double flexiblas_real_LAPACKE_dlapy3(double x, double y, double z);
double flexiblas_chain_LAPACKE_dlapy3(double x, double y, double z);

double flexiblas_real_LAPACKE_dlapy3_work(double x, double y, double z);
double flexiblas_chain_LAPACKE_dlapy3_work(double x, double y, double z);

lapack_int flexiblas_real_LAPACKE_dlarfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dlarfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dlarfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work, lapack_int ldwork);
lapack_int flexiblas_chain_LAPACKE_dlarfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work, lapack_int ldwork);

lapack_int flexiblas_real_LAPACKE_dlarfg(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);
lapack_int flexiblas_chain_LAPACKE_dlarfg(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);

lapack_int flexiblas_real_LAPACKE_dlarfg_work(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);
lapack_int flexiblas_chain_LAPACKE_dlarfg_work(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);

lapack_int flexiblas_real_LAPACKE_dlarft(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dlarft(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dlarft_work(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dlarft_work(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dlarfx(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, double tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_dlarfx(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, double tau, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_dlarfx_work(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, double tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_dlarfx_work(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, double tau, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_dlarnv(lapack_int idist, void *iseed, lapack_int n, void *x);
lapack_int flexiblas_chain_LAPACKE_dlarnv(lapack_int idist, void *iseed, lapack_int n, void *x);

lapack_int flexiblas_real_LAPACKE_dlarnv_work(lapack_int idist, void *iseed, lapack_int n, void *x);
lapack_int flexiblas_chain_LAPACKE_dlarnv_work(lapack_int idist, void *iseed, lapack_int n, void *x);

lapack_int flexiblas_real_LAPACKE_dlartgp(double f, double g, void *cs, void *sn, void *r);
lapack_int flexiblas_chain_LAPACKE_dlartgp(double f, double g, void *cs, void *sn, void *r);

lapack_int flexiblas_real_LAPACKE_dlartgp_work(double f, double g, void *cs, void *sn, void *r);
lapack_int flexiblas_chain_LAPACKE_dlartgp_work(double f, double g, void *cs, void *sn, void *r);

lapack_int flexiblas_real_LAPACKE_dlartgs(double x, double y, double sigma, void *cs, void *sn);
lapack_int flexiblas_chain_LAPACKE_dlartgs(double x, double y, double sigma, void *cs, void *sn);

lapack_int flexiblas_real_LAPACKE_dlartgs_work(double x, double y, double sigma, void *cs, void *sn);
lapack_int flexiblas_chain_LAPACKE_dlartgs_work(double x, double y, double sigma, void *cs, void *sn);

lapack_int flexiblas_real_LAPACKE_dlascl(int matrix_layout, char type, lapack_int kl, lapack_int ku, double cfrom, double cto, lapack_int m, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dlascl(int matrix_layout, char type, lapack_int kl, lapack_int ku, double cfrom, double cto, lapack_int m, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dlascl_work(int matrix_layout, char type, lapack_int kl, lapack_int ku, double cfrom, double cto, lapack_int m, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dlascl_work(int matrix_layout, char type, lapack_int kl, lapack_int ku, double cfrom, double cto, lapack_int m, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dlaset(int matrix_layout, char uplo, lapack_int m, lapack_int n, double alpha, double beta, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dlaset(int matrix_layout, char uplo, lapack_int m, lapack_int n, double alpha, double beta, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dlaset_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, double alpha, double beta, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dlaset_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, double alpha, double beta, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dlasrt(char id, lapack_int n, void *d);
lapack_int flexiblas_chain_LAPACKE_dlasrt(char id, lapack_int n, void *d);

lapack_int flexiblas_real_LAPACKE_dlasrt_work(char id, lapack_int n, void *d);
lapack_int flexiblas_chain_LAPACKE_dlasrt_work(char id, lapack_int n, void *d);

lapack_int flexiblas_real_LAPACKE_dlassq(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);
lapack_int flexiblas_chain_LAPACKE_dlassq(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);

lapack_int flexiblas_real_LAPACKE_dlassq_work(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);
lapack_int flexiblas_chain_LAPACKE_dlassq_work(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);

lapack_int flexiblas_real_LAPACKE_dlaswp(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_dlaswp(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_dlaswp_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_dlaswp_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_dlauum(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dlauum(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dlauum_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dlauum_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dopgtr(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_dopgtr(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_dopgtr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq, void *work);
lapack_int flexiblas_chain_LAPACKE_dopgtr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq, void *work);

lapack_int flexiblas_real_LAPACKE_dopmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dopmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dopmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_dopmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_dorbdb(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2);
lapack_int flexiblas_chain_LAPACKE_dorbdb(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2);

lapack_int flexiblas_real_LAPACKE_dorbdb_work(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dorbdb_work(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dorcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t);
lapack_int flexiblas_chain_LAPACKE_dorcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t);

lapack_int flexiblas_real_LAPACKE_dorcsd2by1(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t);
lapack_int flexiblas_chain_LAPACKE_dorcsd2by1(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t);

lapack_int flexiblas_real_LAPACKE_dorcsd2by1_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dorcsd2by1_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dorcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dorcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dorgbr(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_dorgbr(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_dorgbr_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dorgbr_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dorghr(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_dorghr(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_dorghr_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dorghr_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dorglq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_dorglq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_dorglq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dorglq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dorgql(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_dorgql(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_dorgql_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dorgql_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dorgqr(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_dorgqr(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_dorgqr_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dorgqr_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dorgrq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_dorgrq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_dorgrq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dorgrq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dorgtr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_dorgtr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_dorgtr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dorgtr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dorgtsqr_row(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dorgtsqr_row(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dorgtsqr_row_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dorgtsqr_row_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dorhr_col(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);
lapack_int flexiblas_chain_LAPACKE_dorhr_col(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);

lapack_int flexiblas_real_LAPACKE_dorhr_col_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);
lapack_int flexiblas_chain_LAPACKE_dorhr_col_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);

lapack_int flexiblas_real_LAPACKE_dormbr(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dormbr(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dormbr_work(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dormbr_work(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dormhr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dormhr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dormhr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dormhr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dormlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dormlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dormlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dormlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dormql(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dormql(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dormql_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dormql_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dormqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dormqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dormqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dormqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dormrq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dormrq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dormrq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dormrq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dormrz(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dormrz(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dormrz_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dormrz_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dormtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_dormtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_dormtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dormtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dpbcon(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dpbcon(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_dpbcon_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, double anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dpbcon_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, double anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dpbequ(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_dpbequ(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_dpbequ_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_dpbequ_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_dpbrfs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dpbrfs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dpbrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dpbrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dpbstf(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);
lapack_int flexiblas_chain_LAPACKE_dpbstf(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);

lapack_int flexiblas_real_LAPACKE_dpbstf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);
lapack_int flexiblas_chain_LAPACKE_dpbstf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);

lapack_int flexiblas_real_LAPACKE_dpbsv(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpbsv(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dpbsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpbsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dpbsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dpbsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dpbsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dpbsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dpbtrf(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);
lapack_int flexiblas_chain_LAPACKE_dpbtrf(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);

lapack_int flexiblas_real_LAPACKE_dpbtrf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);
lapack_int flexiblas_chain_LAPACKE_dpbtrf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);

lapack_int flexiblas_real_LAPACKE_dpbtrs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpbtrs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dpbtrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpbtrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dpftrf(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_dpftrf(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_dpftrf_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_dpftrf_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_dpftri(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_dpftri(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_dpftri_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_dpftri_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_dpftrs(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpftrs(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dpftrs_work(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpftrs_work(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dpocon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dpocon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_dpocon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dpocon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dpoequ(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_dpoequ(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_dpoequ_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_dpoequ_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_dpoequb(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_dpoequb(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_dpoequb_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_dpoequb_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_dporfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dporfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dporfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dporfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dposvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dposvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dposvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dposvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dpotrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dpotrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dpotrf2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dpotrf2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dpotrf2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dpotrf2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dpotrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dpotrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dpotri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dpotri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dpotri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dpotri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dpotrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpotrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dpotrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpotrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dppcon(int matrix_layout, char uplo, lapack_int n, const void *ap, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dppcon(int matrix_layout, char uplo, lapack_int n, const void *ap, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_dppcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, double anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dppcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, double anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dppequ(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_dppequ(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_dppequ_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_dppequ_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_dpprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dpprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dpprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dpprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dppsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dppsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dppsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dppsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dppsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dppsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dppsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dppsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dpptrf(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_dpptrf(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_dpptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_dpptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_dpptri(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_dpptri(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_dpptri_work(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_dpptri_work(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_dpptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dpptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dpstrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, double tol);
lapack_int flexiblas_chain_LAPACKE_dpstrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, double tol);

lapack_int flexiblas_real_LAPACKE_dpstrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, double tol, void *work);
lapack_int flexiblas_chain_LAPACKE_dpstrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, double tol, void *work);

lapack_int flexiblas_real_LAPACKE_dptcon(lapack_int n, const void *d, const void *e, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dptcon(lapack_int n, const void *d, const void *e, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_dptcon_work(lapack_int n, const void *d, const void *e, double anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_dptcon_work(lapack_int n, const void *d, const void *e, double anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_dpteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dpteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dpteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_dpteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_dptrfs(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dptrfs(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dptrfs_work(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work);
lapack_int flexiblas_chain_LAPACKE_dptrfs_work(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work);

lapack_int flexiblas_real_LAPACKE_dptsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dptsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dptsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dptsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dptsvx(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dptsvx(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dptsvx_work(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work);
lapack_int flexiblas_chain_LAPACKE_dptsvx_work(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work);

lapack_int flexiblas_real_LAPACKE_dpttrf(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_dpttrf(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_dpttrf_work(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_dpttrf_work(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_dpttrs(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpttrs(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dpttrs_work(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dpttrs_work(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsbev(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dsbev(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dsbev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dsbev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dsbev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsbev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsbev_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_dsbev_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_dsbevd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dsbevd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dsbevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dsbevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dsbevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dsbevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dsbevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dsbevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dsbevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsbevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsbevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsbevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsbevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsbevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsbevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsbevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsbgst(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx);
lapack_int flexiblas_chain_LAPACKE_dsbgst(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx);

lapack_int flexiblas_real_LAPACKE_dsbgst_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx, void *work);
lapack_int flexiblas_chain_LAPACKE_dsbgst_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx, void *work);

lapack_int flexiblas_real_LAPACKE_dsbgv(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dsbgv(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dsbgv_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_dsbgv_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_dsbgvd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dsbgvd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dsbgvd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dsbgvd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dsbgvx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsbgvx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsbgvx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsbgvx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsbtrd(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_dsbtrd(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_dsbtrd_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *work);
lapack_int flexiblas_chain_LAPACKE_dsbtrd_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *work);

lapack_int flexiblas_real_LAPACKE_dsfrk(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, double alpha, const void *a, lapack_int lda, double beta, void *c);
lapack_int flexiblas_chain_LAPACKE_dsfrk(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, double alpha, const void *a, lapack_int lda, double beta, void *c);

lapack_int flexiblas_real_LAPACKE_dsfrk_work(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, double alpha, const void *a, lapack_int lda, double beta, void *c);
lapack_int flexiblas_chain_LAPACKE_dsfrk_work(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, double alpha, const void *a, lapack_int lda, double beta, void *c);

lapack_int flexiblas_real_LAPACKE_dsgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *x, lapack_int ldx, void *iter);
lapack_int flexiblas_chain_LAPACKE_dsgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *x, lapack_int ldx, void *iter);

lapack_int flexiblas_real_LAPACKE_dsgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *x, lapack_int ldx, void *work, void *swork, void *iter);
lapack_int flexiblas_chain_LAPACKE_dsgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *x, lapack_int ldx, void *work, void *swork, void *iter);

lapack_int flexiblas_real_LAPACKE_dspcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dspcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_dspcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dspcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dspev(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dspev(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dspev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_dspev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_dspevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dspevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dspevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dspevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dspevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dspevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_dspevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dspevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_dspgst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);
lapack_int flexiblas_chain_LAPACKE_dspgst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);

lapack_int flexiblas_real_LAPACKE_dspgst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);
lapack_int flexiblas_chain_LAPACKE_dspgst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);

lapack_int flexiblas_real_LAPACKE_dspgv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dspgv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dspgv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_dspgv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_dspgvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dspgvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dspgvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dspgvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dspgvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dspgvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_dspgvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dspgvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *x, lapack_int ldx, void *iter);
lapack_int flexiblas_chain_LAPACKE_dsposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *x, lapack_int ldx, void *iter);

lapack_int flexiblas_real_LAPACKE_dsposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *x, lapack_int ldx, void *work, void *swork, void *iter);
lapack_int flexiblas_chain_LAPACKE_dsposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *x, lapack_int ldx, void *work, void *swork, void *iter);

lapack_int flexiblas_real_LAPACKE_dsprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dsprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dsprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dsprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dspsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dspsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dspsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dspsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dspsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dspsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dspsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dspsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dsptrd(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_dsptrd(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_dsptrd_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_dsptrd_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_dsptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dsptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dsptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dsptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dsptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dsptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_dsptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_dsptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_dsptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dstebz(char range, char order, lapack_int n, double vl, double vu, lapack_int il, lapack_int iu, double abstol, const void *d, const void *e, void *m, void *nsplit, void *w, void *iblock, void *isplit);
lapack_int flexiblas_chain_LAPACKE_dstebz(char range, char order, lapack_int n, double vl, double vu, lapack_int il, lapack_int iu, double abstol, const void *d, const void *e, void *m, void *nsplit, void *w, void *iblock, void *isplit);

lapack_int flexiblas_real_LAPACKE_dstebz_work(char range, char order, lapack_int n, double vl, double vu, lapack_int il, lapack_int iu, double abstol, const void *d, const void *e, void *m, void *nsplit, void *w, void *iblock, void *isplit, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dstebz_work(char range, char order, lapack_int n, double vl, double vu, lapack_int il, lapack_int iu, double abstol, const void *d, const void *e, void *m, void *nsplit, void *w, void *iblock, void *isplit, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dstedc(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dstedc(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dstedc_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dstedc_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dstegr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_dstegr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_dstegr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dstegr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dstein(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *ifailv);
lapack_int flexiblas_chain_LAPACKE_dstein(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *ifailv);

lapack_int flexiblas_real_LAPACKE_dstein_work(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *work, void *iwork, void *ifailv);
lapack_int flexiblas_chain_LAPACKE_dstein_work(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *work, void *iwork, void *ifailv);

lapack_int flexiblas_real_LAPACKE_dstemr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac);
lapack_int flexiblas_chain_LAPACKE_dstemr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac);

lapack_int flexiblas_real_LAPACKE_dstemr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dstemr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dsteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dsteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dsteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_dsteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_dsterf(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_dsterf(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_dsterf_work(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_dsterf_work(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_dstev(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dstev(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dstev_work(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_dstev_work(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_dstevd(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_dstevd(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_dstevd_work(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dstevd_work(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dstevr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_dstevr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_dstevr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dstevr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dstevx(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dstevx(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_dstevx_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dstevx_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsycon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dsycon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_dsycon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dsycon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_dsycon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dsycon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dsycon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dsycon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dsyconv(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);
lapack_int flexiblas_chain_LAPACKE_dsyconv(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);

lapack_int flexiblas_real_LAPACKE_dsyconv_work(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);
lapack_int flexiblas_chain_LAPACKE_dsyconv_work(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);

lapack_int flexiblas_real_LAPACKE_dsyequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_dsyequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_dsyequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);
lapack_int flexiblas_chain_LAPACKE_dsyequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);

lapack_int flexiblas_real_LAPACKE_dsyev(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_dsyev(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_dsyev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_dsyev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_dsyev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsyev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsyev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsyev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsyevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_dsyevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_dsyevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_dsyevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_dsyevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dsyevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dsyevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dsyevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dsyevr(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_dsyevr(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_dsyevr_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_dsyevr_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_dsyevr_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dsyevr_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dsyevr_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dsyevr_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dsyevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsyevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsyevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsyevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsyevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsyevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsyevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsyevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsygst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsygst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsygst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsygst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsygv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_dsygv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_dsygv_2stage(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_dsygv_2stage(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_dsygv_2stage_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsygv_2stage_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsygv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsygv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsygvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_dsygvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_dsygvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dsygvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dsygvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsygvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsygvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_dsygvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_dsyrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dsyrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dsyrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dsyrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dsysv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsysv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsysv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsysv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsysv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsysv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsysv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsysv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsysv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsysv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsysv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsysv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsysv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsysv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsysv_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsysv_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsysv_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsysv_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsysv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsysv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsysvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dsysvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dsysvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dsysvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dsyswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_dsyswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_dsyswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_dsyswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_dsytrd(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_dsytrd(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_dsytrd_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsytrd_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsytrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dsytrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dsytrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dsytrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dsytrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);
lapack_int flexiblas_chain_LAPACKE_dsytrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);

lapack_int flexiblas_real_LAPACKE_dsytrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsytrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsytrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsytrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsytrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dsytrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dsytrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsytrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsytrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dsytrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_dsytrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsytrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsytrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsytrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsytri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dsytri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_dsytri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dsytri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_dsytri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsytri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsytri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_dsytri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_dsytri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_dsytri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_dsytri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_dsytri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_dsytri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsytri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsytri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_dsytri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_dsytrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsytrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsytrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsytrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsytrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);
lapack_int flexiblas_chain_LAPACKE_dsytrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);

lapack_int flexiblas_real_LAPACKE_dsytrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsytrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsytrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsytrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsytrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsytrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsytrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsytrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsytrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsytrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsytrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dsytrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dsytrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsytrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsytrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsytrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dsytrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dsytrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dtbcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dtbcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond);

lapack_int flexiblas_real_LAPACKE_dtbcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dtbcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dtbrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dtbrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dtbrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dtbrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dtbtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dtbtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dtbtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dtbtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dtfsm(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, double alpha, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dtfsm(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, double alpha, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dtfsm_work(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, double alpha, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dtfsm_work(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, double alpha, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dtftri(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_dtftri(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_dtftri_work(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_dtftri_work(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_dtfttp(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);
lapack_int flexiblas_chain_LAPACKE_dtfttp(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);

lapack_int flexiblas_real_LAPACKE_dtfttp_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);
lapack_int flexiblas_chain_LAPACKE_dtfttp_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);

lapack_int flexiblas_real_LAPACKE_dtfttr(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dtfttr(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dtfttr_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dtfttr_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dtgevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_dtgevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_dtgevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work);
lapack_int flexiblas_chain_LAPACKE_dtgevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work);

lapack_int flexiblas_real_LAPACKE_dtgexc(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *ifst, void *ilst);
lapack_int flexiblas_chain_LAPACKE_dtgexc(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *ifst, void *ilst);

lapack_int flexiblas_real_LAPACKE_dtgexc_work(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *ifst, void *ilst, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dtgexc_work(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *ifst, void *ilst, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_dtgsen(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif);
lapack_int flexiblas_chain_LAPACKE_dtgsen(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif);

lapack_int flexiblas_real_LAPACKE_dtgsen_work(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dtgsen_work(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dtgsja(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *ncycle);
lapack_int flexiblas_chain_LAPACKE_dtgsja(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *ncycle);

lapack_int flexiblas_real_LAPACKE_dtgsja_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *ncycle);
lapack_int flexiblas_chain_LAPACKE_dtgsja_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *ncycle);

lapack_int flexiblas_real_LAPACKE_dtgsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_dtgsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_dtgsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dtgsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dtgsyl(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif);
lapack_int flexiblas_chain_LAPACKE_dtgsyl(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif);

lapack_int flexiblas_real_LAPACKE_dtgsyl_work(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dtgsyl_work(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dtpcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dtpcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond);

lapack_int flexiblas_real_LAPACKE_dtpcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dtpcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dtpmqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dtpmqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dtpmqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work);
lapack_int flexiblas_chain_LAPACKE_dtpmqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work);

lapack_int flexiblas_real_LAPACKE_dtpqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dtpqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dtpqrt2(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dtpqrt2(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dtpqrt2_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_dtpqrt2_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_dtpqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt, void *work);
lapack_int flexiblas_chain_LAPACKE_dtpqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt, void *work);

lapack_int flexiblas_real_LAPACKE_dtprfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dtprfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dtprfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int ldwork);
lapack_int flexiblas_chain_LAPACKE_dtprfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int ldwork);

lapack_int flexiblas_real_LAPACKE_dtprfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dtprfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dtprfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dtprfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dtptri(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_dtptri(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_dtptri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_dtptri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_dtptrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dtptrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dtptrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dtptrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dtpttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);
lapack_int flexiblas_chain_LAPACKE_dtpttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);

lapack_int flexiblas_real_LAPACKE_dtpttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);
lapack_int flexiblas_chain_LAPACKE_dtpttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);

lapack_int flexiblas_real_LAPACKE_dtpttr(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dtpttr(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dtpttr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dtpttr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dtrcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond);
lapack_int flexiblas_chain_LAPACKE_dtrcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond);

lapack_int flexiblas_real_LAPACKE_dtrcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dtrcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dtrevc(int matrix_layout, char side, char howmny, void *select, lapack_int n, const void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_dtrevc(int matrix_layout, char side, char howmny, void *select, lapack_int n, const void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_dtrevc_work(int matrix_layout, char side, char howmny, void *select, lapack_int n, const void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work);
lapack_int flexiblas_chain_LAPACKE_dtrevc_work(int matrix_layout, char side, char howmny, void *select, lapack_int n, const void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work);

lapack_int flexiblas_real_LAPACKE_dtrexc(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *ifst, void *ilst);
lapack_int flexiblas_chain_LAPACKE_dtrexc(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *ifst, void *ilst);

lapack_int flexiblas_real_LAPACKE_dtrexc_work(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *ifst, void *ilst, void *work);
lapack_int flexiblas_chain_LAPACKE_dtrexc_work(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *ifst, void *ilst, void *work);

lapack_int flexiblas_real_LAPACKE_dtrrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_dtrrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_dtrrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dtrrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_dtrsen(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *wr, void *wi, void *m, void *s, void *sep);
lapack_int flexiblas_chain_LAPACKE_dtrsen(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *wr, void *wi, void *m, void *s, void *sep);

lapack_int flexiblas_real_LAPACKE_dtrsen_work(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *wr, void *wi, void *m, void *s, void *sep, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_dtrsen_work(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *wr, void *wi, void *m, void *s, void *sep, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_dtrsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_dtrsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_dtrsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m, void *work, lapack_int ldwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_dtrsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m, void *work, lapack_int ldwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_dtrsyl(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);
lapack_int flexiblas_chain_LAPACKE_dtrsyl(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);

lapack_int flexiblas_real_LAPACKE_dtrsyl3(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);
lapack_int flexiblas_chain_LAPACKE_dtrsyl3(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);

lapack_int flexiblas_real_LAPACKE_dtrsyl3_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale, void *iwork, lapack_int liwork, void *swork, lapack_int ldswork);
lapack_int flexiblas_chain_LAPACKE_dtrsyl3_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale, void *iwork, lapack_int liwork, void *swork, lapack_int ldswork);

lapack_int flexiblas_real_LAPACKE_dtrsyl_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);
lapack_int flexiblas_chain_LAPACKE_dtrsyl_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);

lapack_int flexiblas_real_LAPACKE_dtrtri(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dtrtri(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dtrtri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_dtrtri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_dtrtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dtrtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dtrtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_dtrtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_dtrttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);
lapack_int flexiblas_chain_LAPACKE_dtrttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);

lapack_int flexiblas_real_LAPACKE_dtrttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);
lapack_int flexiblas_chain_LAPACKE_dtrttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);

lapack_int flexiblas_real_LAPACKE_dtrttp(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);
lapack_int flexiblas_chain_LAPACKE_dtrttp(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);

lapack_int flexiblas_real_LAPACKE_dtrttp_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);
lapack_int flexiblas_chain_LAPACKE_dtrttp_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);

lapack_int flexiblas_real_LAPACKE_dtzrzf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_dtzrzf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_dtzrzf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_dtzrzf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

int flexiblas_real_LAPACKE_get_nancheck(void);
int flexiblas_chain_LAPACKE_get_nancheck(void);

void flexiblas_real_LAPACKE_ilaver(void *vers_major, void *vers_minor, void *vers_patch);
void flexiblas_chain_LAPACKE_ilaver(void *vers_major, void *vers_minor, void *vers_patch);

lapack_int flexiblas_real_LAPACKE_sbbcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e);
lapack_int flexiblas_chain_LAPACKE_sbbcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e);

lapack_int flexiblas_real_LAPACKE_sbbcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sbbcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sbdsdc(int matrix_layout, char uplo, char compq, lapack_int n, void *d, void *e, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *q, void *iq);
lapack_int flexiblas_chain_LAPACKE_sbdsdc(int matrix_layout, char uplo, char compq, lapack_int n, void *d, void *e, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *q, void *iq);

lapack_int flexiblas_real_LAPACKE_sbdsdc_work(int matrix_layout, char uplo, char compq, lapack_int n, void *d, void *e, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *q, void *iq, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sbdsdc_work(int matrix_layout, char uplo, char compq, lapack_int n, void *d, void *e, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *q, void *iq, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sbdsqr(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sbdsqr(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sbdsqr_work(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_sbdsqr_work(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_sbdsvdx(int matrix_layout, char uplo, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *z, lapack_int ldz, void *superb);
lapack_int flexiblas_chain_LAPACKE_sbdsvdx(int matrix_layout, char uplo, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *z, lapack_int ldz, void *superb);

lapack_int flexiblas_real_LAPACKE_sbdsvdx_work(int matrix_layout, char uplo, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *z, lapack_int ldz, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sbdsvdx_work(int matrix_layout, char uplo, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *z, lapack_int ldz, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sdisna(char job, lapack_int m, lapack_int n, const void *d, void *sep);
lapack_int flexiblas_chain_LAPACKE_sdisna(char job, lapack_int m, lapack_int n, const void *d, void *sep);

lapack_int flexiblas_real_LAPACKE_sdisna_work(char job, lapack_int m, lapack_int n, const void *d, void *sep);
lapack_int flexiblas_chain_LAPACKE_sdisna_work(char job, lapack_int m, lapack_int n, const void *d, void *sep);

void flexiblas_real_LAPACKE_set_nancheck(int flag);
void flexiblas_chain_LAPACKE_set_nancheck(int flag);

lapack_int flexiblas_real_LAPACKE_sgbbrd(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sgbbrd(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sgbbrd_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_sgbbrd_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_sgbcon(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_sgbcon(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_sgbcon_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, float anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgbcon_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, float anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgbequ(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_sgbequ(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_sgbequ_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_sgbequ_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_sgbequb(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_sgbequb(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_sgbequb_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_sgbequb_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_sgbrfs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_sgbrfs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_sgbrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgbrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgbsv(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgbsv(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgbsv_work(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgbsv_work(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgbsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);
lapack_int flexiblas_chain_LAPACKE_sgbsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);

lapack_int flexiblas_real_LAPACKE_sgbsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgbsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgbtrf(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_sgbtrf(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);

lapack_int flexiblas_real_LAPACKE_sgbtrf_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_sgbtrf_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);

lapack_int flexiblas_real_LAPACKE_sgbtrs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgbtrs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgbtrs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgbtrs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgebak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_sgebak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_sgebak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_sgebak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_sgebal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);
lapack_int flexiblas_chain_LAPACKE_sgebal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);

lapack_int flexiblas_real_LAPACKE_sgebal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);
lapack_int flexiblas_chain_LAPACKE_sgebal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);

lapack_int flexiblas_real_LAPACKE_sgebrd(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup);
lapack_int flexiblas_chain_LAPACKE_sgebrd(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup);

lapack_int flexiblas_real_LAPACKE_sgebrd_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgebrd_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgecon(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_sgecon(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_sgecon_work(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgecon_work(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgedmd_work(int matrix_layout, char jobs, char jobz, char jobr, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *reig, void *imeig, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *w, lapack_int ldw, void *s, lapack_int lds, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_sgedmd_work(int matrix_layout, char jobs, char jobz, char jobr, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *reig, void *imeig, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *w, lapack_int ldw, void *s, lapack_int lds, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_sgedmdq_work(int matrix_layout, char jobs, char jobz, char jobr, char jobq, char jobt, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *f, lapack_int ldf, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *reig, void *imeig, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *v, lapack_int ldv, void *s, lapack_int lds, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_sgedmdq_work(int matrix_layout, char jobs, char jobz, char jobr, char jobq, char jobt, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *f, lapack_int ldf, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *reig, void *imeig, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *v, lapack_int ldv, void *s, lapack_int lds, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_sgeequ(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_sgeequ(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_sgeequ_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_sgeequ_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_sgeequb(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_sgeequb(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_sgeequb_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_sgeequb_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_sgees(int matrix_layout, char jobvs, char sort, LAPACK_S_SELECT2 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs);
lapack_int flexiblas_chain_LAPACKE_sgees(int matrix_layout, char jobvs, char sort, LAPACK_S_SELECT2 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs);

lapack_int flexiblas_real_LAPACKE_sgees_work(int matrix_layout, char jobvs, char sort, LAPACK_S_SELECT2 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *work, lapack_int lwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_sgees_work(int matrix_layout, char jobvs, char sort, LAPACK_S_SELECT2 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *work, lapack_int lwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_sgeesx(int matrix_layout, char jobvs, char sort, LAPACK_S_SELECT2 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_sgeesx(int matrix_layout, char jobvs, char sort, LAPACK_S_SELECT2 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_sgeesx_work(int matrix_layout, char jobvs, char sort, LAPACK_S_SELECT2 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, lapack_int liwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_sgeesx_work(int matrix_layout, char jobvs, char sort, LAPACK_S_SELECT2 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *wr, void *wi, void *vs, lapack_int ldvs, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, lapack_int liwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_sgeev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_sgeev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_sgeev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgeev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgeevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_sgeevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_sgeevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgeevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *wr, void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgehrd(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_sgehrd(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_sgehrd_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgehrd_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgejsv(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *stat, void *istat);
lapack_int flexiblas_chain_LAPACKE_sgejsv(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *stat, void *istat);

lapack_int flexiblas_real_LAPACKE_sgejsv_work(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgejsv_work(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgelq(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);
lapack_int flexiblas_chain_LAPACKE_sgelq(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);

lapack_int flexiblas_real_LAPACKE_sgelq2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_sgelq2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_sgelq2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_sgelq2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_sgelq_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgelq_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgelqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_sgelqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_sgelqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgelqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgels(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgels(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgels_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgels_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgelsd(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_sgelsd(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_sgelsd_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgelsd_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgelss(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_sgelss(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_sgelss_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgelss_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, float rcond, void *rank, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgelsy(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, float rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_sgelsy(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, float rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_sgelsy_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, float rcond, void *rank, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgelsy_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, float rcond, void *rank, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgemlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sgemlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sgemlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgemlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgemqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sgemqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sgemqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgemqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgemqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sgemqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sgemqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_sgemqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_sgeqlf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_sgeqlf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_sgeqlf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgeqlf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgeqp3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);
lapack_int flexiblas_chain_LAPACKE_sgeqp3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);

lapack_int flexiblas_real_LAPACKE_sgeqp3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgeqp3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgeqpf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);
lapack_int flexiblas_chain_LAPACKE_sgeqpf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);

lapack_int flexiblas_real_LAPACKE_sgeqpf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_sgeqpf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_sgeqr(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);
lapack_int flexiblas_chain_LAPACKE_sgeqr(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);

lapack_int flexiblas_real_LAPACKE_sgeqr2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_sgeqr2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_sgeqr2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_sgeqr2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_sgeqr_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgeqr_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgeqrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_sgeqrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_sgeqrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgeqrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgeqrfp(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_sgeqrfp(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_sgeqrfp_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgeqrfp_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgeqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_sgeqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_sgeqrt2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_sgeqrt2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_sgeqrt2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_sgeqrt2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_sgeqrt3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_sgeqrt3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_sgeqrt3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_sgeqrt3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_sgeqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *work);
lapack_int flexiblas_chain_LAPACKE_sgeqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *work);

lapack_int flexiblas_real_LAPACKE_sgerfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_sgerfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_sgerfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgerfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgerqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_sgerqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_sgerqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgerqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgesdd(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt);
lapack_int flexiblas_chain_LAPACKE_sgesdd(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt);

lapack_int flexiblas_real_LAPACKE_sgesdd_work(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgesdd_work(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgesvd(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);
lapack_int flexiblas_chain_LAPACKE_sgesvd(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);

lapack_int flexiblas_real_LAPACKE_sgesvd_work(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgesvd_work(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgesvdq(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank);
lapack_int flexiblas_chain_LAPACKE_sgesvdq(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank);

lapack_int flexiblas_real_LAPACKE_sgesvdq_work(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank, void *iwork, lapack_int liwork, void *work, lapack_int lwork, void *rwork, lapack_int lrwork);
lapack_int flexiblas_chain_LAPACKE_sgesvdq_work(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank, void *iwork, lapack_int liwork, void *work, lapack_int lwork, void *rwork, lapack_int lrwork);

lapack_int flexiblas_real_LAPACKE_sgesvdx(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);
lapack_int flexiblas_chain_LAPACKE_sgesvdx(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);

lapack_int flexiblas_real_LAPACKE_sgesvdx_work(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgesvdx_work(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgesvj(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *stat);
lapack_int flexiblas_chain_LAPACKE_sgesvj(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *stat);

lapack_int flexiblas_real_LAPACKE_sgesvj_work(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgesvj_work(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgesvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);
lapack_int flexiblas_chain_LAPACKE_sgesvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);

lapack_int flexiblas_real_LAPACKE_sgesvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgesvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgetf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_sgetf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_sgetf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_sgetf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_sgetrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_sgetrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_sgetrf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_sgetrf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_sgetrf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_sgetrf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_sgetrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_sgetrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_sgetri(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_sgetri(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_sgetri_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgetri_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgetrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgetrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgetrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgetrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgetsls(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgetsls(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgetsls_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgetsls_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgetsqrhrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_sgetsqrhrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_sgetsqrhrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgetsqrhrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sggbak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_sggbak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_sggbak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_sggbak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_sggbal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale);
lapack_int flexiblas_chain_LAPACKE_sggbal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale);

lapack_int flexiblas_real_LAPACKE_sggbal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale, void *work);
lapack_int flexiblas_chain_LAPACKE_sggbal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale, void *work);

lapack_int flexiblas_real_LAPACKE_sgges(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);
lapack_int flexiblas_chain_LAPACKE_sgges(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);

lapack_int flexiblas_real_LAPACKE_sgges3(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);
lapack_int flexiblas_chain_LAPACKE_sgges3(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);

lapack_int flexiblas_real_LAPACKE_sgges3_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_sgges3_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_sgges_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_sgges_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_sggesx(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_sggesx(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_sggesx_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, lapack_int liwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_sggesx_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_S_SELECT3 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alphar, void *alphai, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, lapack_int liwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_sggev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_sggev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_sggev3(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_sggev3(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_sggev3_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sggev3_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sggev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sggev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sggevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_sggevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_sggevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_sggevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *iwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_sggglm(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y);
lapack_int flexiblas_chain_LAPACKE_sggglm(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y);

lapack_int flexiblas_real_LAPACKE_sggglm_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sggglm_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgghd3(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_sgghd3(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_sgghd3_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgghd3_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sgghrd(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_sgghrd(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_sgghrd_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_sgghrd_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_sgglse(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x);
lapack_int flexiblas_chain_LAPACKE_sgglse(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x);

lapack_int flexiblas_real_LAPACKE_sgglse_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sgglse_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sggqrf(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);
lapack_int flexiblas_chain_LAPACKE_sggqrf(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);

lapack_int flexiblas_real_LAPACKE_sggqrf_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sggqrf_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sggrqf(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);
lapack_int flexiblas_chain_LAPACKE_sggrqf(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);

lapack_int flexiblas_real_LAPACKE_sggrqf_work(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sggrqf_work(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sggsvd(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sggsvd(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);

lapack_int flexiblas_real_LAPACKE_sggsvd3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sggsvd3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);

lapack_int flexiblas_real_LAPACKE_sggsvd3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sggsvd3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_sggsvd_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sggsvd_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sggsvp(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_sggsvp(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_sggsvp3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_sggsvp3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_sggsvp3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sggsvp3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sggsvp_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_sggsvp_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_sgtcon(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_sgtcon(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_sgtcon_work(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, float anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgtcon_work(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, float anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgtrfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_sgtrfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_sgtrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgtrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgtsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgtsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgtsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgtsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgtsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_sgtsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_sgtsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sgtsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sgttrf(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_sgttrf(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);

lapack_int flexiblas_real_LAPACKE_sgttrf_work(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_sgttrf_work(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);

lapack_int flexiblas_real_LAPACKE_sgttrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgttrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sgttrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sgttrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_shgeqz(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_shgeqz(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_shgeqz_work(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_shgeqz_work(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_shsein(int matrix_layout, char job, char eigsrc, char initv, void *select, lapack_int n, const void *h, lapack_int ldh, void *wr, const void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *ifaill, void *ifailr);
lapack_int flexiblas_chain_LAPACKE_shsein(int matrix_layout, char job, char eigsrc, char initv, void *select, lapack_int n, const void *h, lapack_int ldh, void *wr, const void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *ifaill, void *ifailr);

lapack_int flexiblas_real_LAPACKE_shsein_work(int matrix_layout, char job, char eigsrc, char initv, void *select, lapack_int n, const void *h, lapack_int ldh, void *wr, const void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *ifaill, void *ifailr);
lapack_int flexiblas_chain_LAPACKE_shsein_work(int matrix_layout, char job, char eigsrc, char initv, void *select, lapack_int n, const void *h, lapack_int ldh, void *wr, const void *wi, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *ifaill, void *ifailr);

lapack_int flexiblas_real_LAPACKE_shseqr(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *wr, void *wi, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_shseqr(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *wr, void *wi, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_shseqr_work(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *wr, void *wi, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_shseqr_work(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *wr, void *wi, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_slacn2(lapack_int n, void *v, void *x, void *isgn, void *est, void *kase, void *isave);
lapack_int flexiblas_chain_LAPACKE_slacn2(lapack_int n, void *v, void *x, void *isgn, void *est, void *kase, void *isave);

lapack_int flexiblas_real_LAPACKE_slacn2_work(lapack_int n, void *v, void *x, void *isgn, void *est, void *kase, void *isave);
lapack_int flexiblas_chain_LAPACKE_slacn2_work(lapack_int n, void *v, void *x, void *isgn, void *est, void *kase, void *isave);

lapack_int flexiblas_real_LAPACKE_slacpy(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_slacpy(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_slacpy_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_slacpy_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_slag2d(int matrix_layout, lapack_int m, lapack_int n, const void *sa, lapack_int ldsa, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_slag2d(int matrix_layout, lapack_int m, lapack_int n, const void *sa, lapack_int ldsa, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_slag2d_work(int matrix_layout, lapack_int m, lapack_int n, const void *sa, lapack_int ldsa, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_slag2d_work(int matrix_layout, lapack_int m, lapack_int n, const void *sa, lapack_int ldsa, void *a, lapack_int lda);

float flexiblas_real_LAPACKE_slamch(char cmach);
float flexiblas_chain_LAPACKE_slamch(char cmach);

float flexiblas_real_LAPACKE_slamch_work(char cmach);
float flexiblas_chain_LAPACKE_slamch_work(char cmach);

float flexiblas_real_LAPACKE_slangb(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab);
float flexiblas_chain_LAPACKE_slangb(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab);

float flexiblas_real_LAPACKE_slangb_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *work);
float flexiblas_chain_LAPACKE_slangb_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *work);

float flexiblas_real_LAPACKE_slange(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda);
float flexiblas_chain_LAPACKE_slange(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda);

float flexiblas_real_LAPACKE_slange_work(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);
float flexiblas_chain_LAPACKE_slange_work(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);

float flexiblas_real_LAPACKE_slansy(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);
float flexiblas_chain_LAPACKE_slansy(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);

float flexiblas_real_LAPACKE_slansy_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);
float flexiblas_chain_LAPACKE_slansy_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);

float flexiblas_real_LAPACKE_slantr(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda);
float flexiblas_chain_LAPACKE_slantr(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda);

float flexiblas_real_LAPACKE_slantr_work(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);
float flexiblas_chain_LAPACKE_slantr_work(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);

lapack_int flexiblas_real_LAPACKE_slapmr(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_slapmr(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_slapmr_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_slapmr_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_slapmt(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_slapmt(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_slapmt_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_slapmt_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

float flexiblas_real_LAPACKE_slapy2(float x, float y);
float flexiblas_chain_LAPACKE_slapy2(float x, float y);

float flexiblas_real_LAPACKE_slapy2_work(float x, float y);
float flexiblas_chain_LAPACKE_slapy2_work(float x, float y);

float flexiblas_real_LAPACKE_slapy3(float x, float y, float z);
float flexiblas_chain_LAPACKE_slapy3(float x, float y, float z);

float flexiblas_real_LAPACKE_slapy3_work(float x, float y, float z);
float flexiblas_chain_LAPACKE_slapy3_work(float x, float y, float z);

lapack_int flexiblas_real_LAPACKE_slarfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_slarfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_slarfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work, lapack_int ldwork);
lapack_int flexiblas_chain_LAPACKE_slarfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work, lapack_int ldwork);

lapack_int flexiblas_real_LAPACKE_slarfg(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);
lapack_int flexiblas_chain_LAPACKE_slarfg(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);

lapack_int flexiblas_real_LAPACKE_slarfg_work(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);
lapack_int flexiblas_chain_LAPACKE_slarfg_work(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);

lapack_int flexiblas_real_LAPACKE_slarft(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_slarft(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_slarft_work(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_slarft_work(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_slarfx(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, float tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_slarfx(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, float tau, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_slarfx_work(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, float tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_slarfx_work(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, float tau, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_slarnv(lapack_int idist, void *iseed, lapack_int n, void *x);
lapack_int flexiblas_chain_LAPACKE_slarnv(lapack_int idist, void *iseed, lapack_int n, void *x);

lapack_int flexiblas_real_LAPACKE_slarnv_work(lapack_int idist, void *iseed, lapack_int n, void *x);
lapack_int flexiblas_chain_LAPACKE_slarnv_work(lapack_int idist, void *iseed, lapack_int n, void *x);

lapack_int flexiblas_real_LAPACKE_slartgp(float f, float g, void *cs, void *sn, void *r);
lapack_int flexiblas_chain_LAPACKE_slartgp(float f, float g, void *cs, void *sn, void *r);

lapack_int flexiblas_real_LAPACKE_slartgp_work(float f, float g, void *cs, void *sn, void *r);
lapack_int flexiblas_chain_LAPACKE_slartgp_work(float f, float g, void *cs, void *sn, void *r);

lapack_int flexiblas_real_LAPACKE_slartgs(float x, float y, float sigma, void *cs, void *sn);
lapack_int flexiblas_chain_LAPACKE_slartgs(float x, float y, float sigma, void *cs, void *sn);

lapack_int flexiblas_real_LAPACKE_slartgs_work(float x, float y, float sigma, void *cs, void *sn);
lapack_int flexiblas_chain_LAPACKE_slartgs_work(float x, float y, float sigma, void *cs, void *sn);

lapack_int flexiblas_real_LAPACKE_slascl(int matrix_layout, char type, lapack_int kl, lapack_int ku, float cfrom, float cto, lapack_int m, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_slascl(int matrix_layout, char type, lapack_int kl, lapack_int ku, float cfrom, float cto, lapack_int m, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_slascl_work(int matrix_layout, char type, lapack_int kl, lapack_int ku, float cfrom, float cto, lapack_int m, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_slascl_work(int matrix_layout, char type, lapack_int kl, lapack_int ku, float cfrom, float cto, lapack_int m, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_slaset(int matrix_layout, char uplo, lapack_int m, lapack_int n, float alpha, float beta, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_slaset(int matrix_layout, char uplo, lapack_int m, lapack_int n, float alpha, float beta, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_slaset_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, float alpha, float beta, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_slaset_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, float alpha, float beta, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_slasrt(char id, lapack_int n, void *d);
lapack_int flexiblas_chain_LAPACKE_slasrt(char id, lapack_int n, void *d);

lapack_int flexiblas_real_LAPACKE_slasrt_work(char id, lapack_int n, void *d);
lapack_int flexiblas_chain_LAPACKE_slasrt_work(char id, lapack_int n, void *d);

lapack_int flexiblas_real_LAPACKE_slassq(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);
lapack_int flexiblas_chain_LAPACKE_slassq(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);

lapack_int flexiblas_real_LAPACKE_slassq_work(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);
lapack_int flexiblas_chain_LAPACKE_slassq_work(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);

lapack_int flexiblas_real_LAPACKE_slaswp(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_slaswp(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_slaswp_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_slaswp_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_slauum(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_slauum(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_slauum_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_slauum_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_sopgtr(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_sopgtr(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_sopgtr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq, void *work);
lapack_int flexiblas_chain_LAPACKE_sopgtr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq, void *work);

lapack_int flexiblas_real_LAPACKE_sopmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sopmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sopmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_sopmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_sorbdb(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2);
lapack_int flexiblas_chain_LAPACKE_sorbdb(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2);

lapack_int flexiblas_real_LAPACKE_sorbdb_work(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sorbdb_work(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sorcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t);
lapack_int flexiblas_chain_LAPACKE_sorcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t);

lapack_int flexiblas_real_LAPACKE_sorcsd2by1(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t);
lapack_int flexiblas_chain_LAPACKE_sorcsd2by1(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t);

lapack_int flexiblas_real_LAPACKE_sorcsd2by1_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sorcsd2by1_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_sorcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sorcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_sorgbr(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_sorgbr(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_sorgbr_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sorgbr_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sorghr(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_sorghr(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_sorghr_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sorghr_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sorglq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_sorglq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_sorglq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sorglq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sorgql(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_sorgql(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_sorgql_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sorgql_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sorgqr(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_sorgqr(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_sorgqr_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sorgqr_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sorgrq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_sorgrq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_sorgrq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sorgrq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sorgtr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_sorgtr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_sorgtr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sorgtr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sorgtsqr_row(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_sorgtsqr_row(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_sorgtsqr_row_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sorgtsqr_row_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sorhr_col(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);
lapack_int flexiblas_chain_LAPACKE_sorhr_col(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);

lapack_int flexiblas_real_LAPACKE_sorhr_col_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);
lapack_int flexiblas_chain_LAPACKE_sorhr_col_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);

lapack_int flexiblas_real_LAPACKE_sormbr(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sormbr(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sormbr_work(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sormbr_work(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sormhr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sormhr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sormhr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sormhr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sormlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sormlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sormlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sormlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sormql(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sormql(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sormql_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sormql_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sormqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sormqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sormqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sormqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sormrq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sormrq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sormrq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sormrq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sormrz(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sormrz(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sormrz_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sormrz_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_sormtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_sormtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_sormtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_sormtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_spbcon(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_spbcon(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_spbcon_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, float anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_spbcon_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, float anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_spbequ(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_spbequ(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_spbequ_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_spbequ_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_spbrfs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_spbrfs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_spbrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_spbrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_spbstf(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);
lapack_int flexiblas_chain_LAPACKE_spbstf(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);

lapack_int flexiblas_real_LAPACKE_spbstf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);
lapack_int flexiblas_chain_LAPACKE_spbstf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);

lapack_int flexiblas_real_LAPACKE_spbsv(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spbsv(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_spbsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spbsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_spbsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_spbsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_spbsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_spbsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_spbtrf(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);
lapack_int flexiblas_chain_LAPACKE_spbtrf(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);

lapack_int flexiblas_real_LAPACKE_spbtrf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);
lapack_int flexiblas_chain_LAPACKE_spbtrf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);

lapack_int flexiblas_real_LAPACKE_spbtrs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spbtrs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_spbtrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spbtrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_spftrf(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_spftrf(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_spftrf_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_spftrf_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_spftri(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_spftri(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_spftri_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_spftri_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_spftrs(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spftrs(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_spftrs_work(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spftrs_work(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_spocon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_spocon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_spocon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_spocon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, float anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_spoequ(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_spoequ(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_spoequ_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_spoequ_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_spoequb(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_spoequb(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_spoequb_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_spoequb_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_sporfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_sporfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_sporfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sporfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sposvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_sposvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_sposvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sposvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_spotrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_spotrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_spotrf2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_spotrf2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_spotrf2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_spotrf2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_spotrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_spotrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_spotri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_spotri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_spotri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_spotri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_spotrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spotrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_spotrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spotrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sppcon(int matrix_layout, char uplo, lapack_int n, const void *ap, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_sppcon(int matrix_layout, char uplo, lapack_int n, const void *ap, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_sppcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, float anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sppcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, float anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sppequ(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_sppequ(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_sppequ_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_sppequ_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_spprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_spprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_spprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_spprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sppsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sppsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sppsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sppsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sppsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_sppsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_sppsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sppsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_spptrf(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_spptrf(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_spptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_spptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_spptri(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_spptri(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_spptri_work(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_spptri_work(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_spptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_spptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_spstrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, float tol);
lapack_int flexiblas_chain_LAPACKE_spstrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, float tol);

lapack_int flexiblas_real_LAPACKE_spstrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, float tol, void *work);
lapack_int flexiblas_chain_LAPACKE_spstrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, float tol, void *work);

lapack_int flexiblas_real_LAPACKE_sptcon(lapack_int n, const void *d, const void *e, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_sptcon(lapack_int n, const void *d, const void *e, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_sptcon_work(lapack_int n, const void *d, const void *e, float anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_sptcon_work(lapack_int n, const void *d, const void *e, float anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_spteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_spteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_spteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_spteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_sptrfs(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_sptrfs(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_sptrfs_work(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work);
lapack_int flexiblas_chain_LAPACKE_sptrfs_work(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work);

lapack_int flexiblas_real_LAPACKE_sptsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sptsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sptsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sptsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sptsvx(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_sptsvx(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_sptsvx_work(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work);
lapack_int flexiblas_chain_LAPACKE_sptsvx_work(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work);

lapack_int flexiblas_real_LAPACKE_spttrf(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_spttrf(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_spttrf_work(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_spttrf_work(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_spttrs(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spttrs(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_spttrs_work(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_spttrs_work(int matrix_layout, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssbev(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_ssbev(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_ssbev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_ssbev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_ssbev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssbev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssbev_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_ssbev_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_ssbevd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_ssbevd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_ssbevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_ssbevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_ssbevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_ssbevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_ssbevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_ssbevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_ssbevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssbevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssbevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssbevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssbevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssbevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssbevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssbevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssbgst(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx);
lapack_int flexiblas_chain_LAPACKE_ssbgst(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx);

lapack_int flexiblas_real_LAPACKE_ssbgst_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx, void *work);
lapack_int flexiblas_chain_LAPACKE_ssbgst_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx, void *work);

lapack_int flexiblas_real_LAPACKE_ssbgv(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_ssbgv(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_ssbgv_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_ssbgv_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_ssbgvd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_ssbgvd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_ssbgvd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_ssbgvd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_ssbgvx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssbgvx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssbgvx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssbgvx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssbtrd(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_ssbtrd(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_ssbtrd_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *work);
lapack_int flexiblas_chain_LAPACKE_ssbtrd_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *work);

lapack_int flexiblas_real_LAPACKE_ssfrk(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, float alpha, const void *a, lapack_int lda, float beta, void *c);
lapack_int flexiblas_chain_LAPACKE_ssfrk(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, float alpha, const void *a, lapack_int lda, float beta, void *c);

lapack_int flexiblas_real_LAPACKE_ssfrk_work(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, float alpha, const void *a, lapack_int lda, float beta, void *c);
lapack_int flexiblas_chain_LAPACKE_ssfrk_work(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, float alpha, const void *a, lapack_int lda, float beta, void *c);

lapack_int flexiblas_real_LAPACKE_sspcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_sspcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_sspcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sspcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, float anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sspev(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_sspev(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_sspev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_sspev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_sspevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_sspevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_sspevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_sspevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_sspevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_sspevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_sspevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_sspevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_sspgst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);
lapack_int flexiblas_chain_LAPACKE_sspgst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);

lapack_int flexiblas_real_LAPACKE_sspgst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);
lapack_int flexiblas_chain_LAPACKE_sspgst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);

lapack_int flexiblas_real_LAPACKE_sspgv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_sspgv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_sspgv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_sspgv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_sspgvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_sspgvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_sspgvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_sspgvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_sspgvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_sspgvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_sspgvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_sspgvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_ssprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_ssprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_ssprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sspsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sspsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sspsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_sspsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sspsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_sspsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_sspsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sspsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_ssptrd(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_ssptrd(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_ssptrd_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_ssptrd_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_ssptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_ssptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_ssptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_ssptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_ssptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_ssptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_ssptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_ssptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_ssptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_sstebz(char range, char order, lapack_int n, float vl, float vu, lapack_int il, lapack_int iu, float abstol, const void *d, const void *e, void *m, void *nsplit, void *w, void *iblock, void *isplit);
lapack_int flexiblas_chain_LAPACKE_sstebz(char range, char order, lapack_int n, float vl, float vu, lapack_int il, lapack_int iu, float abstol, const void *d, const void *e, void *m, void *nsplit, void *w, void *iblock, void *isplit);

lapack_int flexiblas_real_LAPACKE_sstebz_work(char range, char order, lapack_int n, float vl, float vu, lapack_int il, lapack_int iu, float abstol, const void *d, const void *e, void *m, void *nsplit, void *w, void *iblock, void *isplit, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_sstebz_work(char range, char order, lapack_int n, float vl, float vu, lapack_int il, lapack_int iu, float abstol, const void *d, const void *e, void *m, void *nsplit, void *w, void *iblock, void *isplit, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_sstedc(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_sstedc(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_sstedc_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_sstedc_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_sstegr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_sstegr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_sstegr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_sstegr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_sstein(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *ifailv);
lapack_int flexiblas_chain_LAPACKE_sstein(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *ifailv);

lapack_int flexiblas_real_LAPACKE_sstein_work(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *work, void *iwork, void *ifailv);
lapack_int flexiblas_chain_LAPACKE_sstein_work(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *work, void *iwork, void *ifailv);

lapack_int flexiblas_real_LAPACKE_sstemr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac);
lapack_int flexiblas_chain_LAPACKE_sstemr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac);

lapack_int flexiblas_real_LAPACKE_sstemr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_sstemr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_ssteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_ssteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_ssteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_ssteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_ssterf(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_ssterf(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_ssterf_work(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_ssterf_work(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_sstev(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_sstev(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_sstev_work(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_sstev_work(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_sstevd(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_sstevd(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_sstevd_work(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_sstevd_work(int matrix_layout, char jobz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_sstevr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_sstevr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_sstevr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_sstevr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_sstevx(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_sstevx(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_sstevx_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_sstevx_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssycon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_ssycon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_ssycon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_ssycon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_ssycon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_ssycon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, float anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_ssycon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_ssycon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, float anorm, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_ssyconv(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);
lapack_int flexiblas_chain_LAPACKE_ssyconv(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);

lapack_int flexiblas_real_LAPACKE_ssyconv_work(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);
lapack_int flexiblas_chain_LAPACKE_ssyconv_work(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);

lapack_int flexiblas_real_LAPACKE_ssyequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_ssyequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_ssyequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);
lapack_int flexiblas_chain_LAPACKE_ssyequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);

lapack_int flexiblas_real_LAPACKE_ssyev(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_ssyev(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_ssyev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_ssyev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_ssyev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssyev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssyev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssyev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssyevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_ssyevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_ssyevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_ssyevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_ssyevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_ssyevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_ssyevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_ssyevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_ssyevr(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_ssyevr(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_ssyevr_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_ssyevr_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_ssyevr_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_ssyevr_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_ssyevr_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_ssyevr_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_ssyevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssyevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssyevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssyevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssyevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssyevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssyevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssyevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssygst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssygst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssygst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssygst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssygv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_ssygv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_ssygv_2stage(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_ssygv_2stage(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_ssygv_2stage_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssygv_2stage_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssygv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssygv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssygvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_ssygvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_ssygvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_ssygvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_ssygvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssygvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssygvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_ssygvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, float vl, float vu, lapack_int il, lapack_int iu, float abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_ssyrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_ssyrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_ssyrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_ssyrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_ssysv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssysv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssysv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssysv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssysv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssysv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssysv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssysv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssysv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssysv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssysv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssysv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssysv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssysv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssysv_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssysv_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssysv_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssysv_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssysv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssysv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssysvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_ssysvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_ssysvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_ssysvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_ssyswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_ssyswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_ssyswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_ssyswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_ssytrd(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_ssytrd(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_ssytrd_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssytrd_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssytrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_ssytrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_ssytrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_ssytrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_ssytrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);
lapack_int flexiblas_chain_LAPACKE_ssytrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);

lapack_int flexiblas_real_LAPACKE_ssytrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssytrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssytrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssytrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssytrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_ssytrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);

lapack_int flexiblas_real_LAPACKE_ssytrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssytrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssytrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_ssytrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_ssytrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssytrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssytrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssytrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssytri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_ssytri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_ssytri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_ssytri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_ssytri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssytri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssytri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_ssytri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_ssytri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_ssytri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_ssytri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_ssytri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_ssytri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssytri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssytri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_ssytri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_ssytrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssytrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssytrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssytrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssytrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);
lapack_int flexiblas_chain_LAPACKE_ssytrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);

lapack_int flexiblas_real_LAPACKE_ssytrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssytrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssytrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssytrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssytrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssytrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssytrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssytrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssytrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssytrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssytrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ssytrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ssytrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssytrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssytrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssytrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ssytrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ssytrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_stbcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond);
lapack_int flexiblas_chain_LAPACKE_stbcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond);

lapack_int flexiblas_real_LAPACKE_stbcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_stbcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_stbrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_stbrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_stbrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_stbrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_stbtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_stbtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_stbtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_stbtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_stfsm(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, float alpha, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_stfsm(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, float alpha, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_stfsm_work(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, float alpha, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_stfsm_work(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, float alpha, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_stftri(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_stftri(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_stftri_work(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_stftri_work(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_stfttp(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);
lapack_int flexiblas_chain_LAPACKE_stfttp(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);

lapack_int flexiblas_real_LAPACKE_stfttp_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);
lapack_int flexiblas_chain_LAPACKE_stfttp_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);

lapack_int flexiblas_real_LAPACKE_stfttr(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_stfttr(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_stfttr_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_stfttr_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_stgevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_stgevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_stgevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work);
lapack_int flexiblas_chain_LAPACKE_stgevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work);

lapack_int flexiblas_real_LAPACKE_stgexc(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *ifst, void *ilst);
lapack_int flexiblas_chain_LAPACKE_stgexc(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *ifst, void *ilst);

lapack_int flexiblas_real_LAPACKE_stgexc_work(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *ifst, void *ilst, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_stgexc_work(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *ifst, void *ilst, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_stgsen(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif);
lapack_int flexiblas_chain_LAPACKE_stgsen(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif);

lapack_int flexiblas_real_LAPACKE_stgsen_work(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_stgsen_work(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alphar, void *alphai, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_stgsja(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *ncycle);
lapack_int flexiblas_chain_LAPACKE_stgsja(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *ncycle);

lapack_int flexiblas_real_LAPACKE_stgsja_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *ncycle);
lapack_int flexiblas_chain_LAPACKE_stgsja_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, float tola, float tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *ncycle);

lapack_int flexiblas_real_LAPACKE_stgsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_stgsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_stgsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_stgsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_stgsyl(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif);
lapack_int flexiblas_chain_LAPACKE_stgsyl(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif);

lapack_int flexiblas_real_LAPACKE_stgsyl_work(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_stgsyl_work(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_stpcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond);
lapack_int flexiblas_chain_LAPACKE_stpcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond);

lapack_int flexiblas_real_LAPACKE_stpcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_stpcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_stpmqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_stpmqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_stpmqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work);
lapack_int flexiblas_chain_LAPACKE_stpmqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work);

lapack_int flexiblas_real_LAPACKE_stpqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_stpqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_stpqrt2(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_stpqrt2(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_stpqrt2_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_stpqrt2_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_stpqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt, void *work);
lapack_int flexiblas_chain_LAPACKE_stpqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt, void *work);

lapack_int flexiblas_real_LAPACKE_stprfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_stprfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_stprfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int ldwork);
lapack_int flexiblas_chain_LAPACKE_stprfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int ldwork);

lapack_int flexiblas_real_LAPACKE_stprfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_stprfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_stprfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_stprfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_stptri(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_stptri(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_stptri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_stptri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_stptrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_stptrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_stptrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_stptrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_stpttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);
lapack_int flexiblas_chain_LAPACKE_stpttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);

lapack_int flexiblas_real_LAPACKE_stpttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);
lapack_int flexiblas_chain_LAPACKE_stpttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);

lapack_int flexiblas_real_LAPACKE_stpttr(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_stpttr(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_stpttr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_stpttr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_strcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond);
lapack_int flexiblas_chain_LAPACKE_strcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond);

lapack_int flexiblas_real_LAPACKE_strcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_strcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_strevc(int matrix_layout, char side, char howmny, void *select, lapack_int n, const void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_strevc(int matrix_layout, char side, char howmny, void *select, lapack_int n, const void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_strevc_work(int matrix_layout, char side, char howmny, void *select, lapack_int n, const void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work);
lapack_int flexiblas_chain_LAPACKE_strevc_work(int matrix_layout, char side, char howmny, void *select, lapack_int n, const void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work);

lapack_int flexiblas_real_LAPACKE_strexc(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *ifst, void *ilst);
lapack_int flexiblas_chain_LAPACKE_strexc(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *ifst, void *ilst);

lapack_int flexiblas_real_LAPACKE_strexc_work(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *ifst, void *ilst, void *work);
lapack_int flexiblas_chain_LAPACKE_strexc_work(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *ifst, void *ilst, void *work);

lapack_int flexiblas_real_LAPACKE_strrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_strrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_strrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);
lapack_int flexiblas_chain_LAPACKE_strrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *iwork);

lapack_int flexiblas_real_LAPACKE_strsen(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *wr, void *wi, void *m, void *s, void *sep);
lapack_int flexiblas_chain_LAPACKE_strsen(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *wr, void *wi, void *m, void *s, void *sep);

lapack_int flexiblas_real_LAPACKE_strsen_work(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *wr, void *wi, void *m, void *s, void *sep, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_strsen_work(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *wr, void *wi, void *m, void *s, void *sep, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_strsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_strsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_strsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m, void *work, lapack_int ldwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_strsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m, void *work, lapack_int ldwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_strsyl(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);
lapack_int flexiblas_chain_LAPACKE_strsyl(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);

lapack_int flexiblas_real_LAPACKE_strsyl3(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);
lapack_int flexiblas_chain_LAPACKE_strsyl3(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);

lapack_int flexiblas_real_LAPACKE_strsyl3_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale, void *iwork, lapack_int liwork, void *swork, lapack_int ldswork);
lapack_int flexiblas_chain_LAPACKE_strsyl3_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale, void *iwork, lapack_int liwork, void *swork, lapack_int ldswork);

lapack_int flexiblas_real_LAPACKE_strsyl_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);
lapack_int flexiblas_chain_LAPACKE_strsyl_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);

lapack_int flexiblas_real_LAPACKE_strtri(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_strtri(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_strtri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_strtri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_strtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_strtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_strtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_strtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_strttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);
lapack_int flexiblas_chain_LAPACKE_strttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);

lapack_int flexiblas_real_LAPACKE_strttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);
lapack_int flexiblas_chain_LAPACKE_strttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);

lapack_int flexiblas_real_LAPACKE_strttp(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);
lapack_int flexiblas_chain_LAPACKE_strttp(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);

lapack_int flexiblas_real_LAPACKE_strttp_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);
lapack_int flexiblas_chain_LAPACKE_strttp_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);

lapack_int flexiblas_real_LAPACKE_stzrzf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_stzrzf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_stzrzf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_stzrzf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zbbcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e);
lapack_int flexiblas_chain_LAPACKE_zbbcsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e);

lapack_int flexiblas_real_LAPACKE_zbbcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e, void *rwork, lapack_int lrwork);
lapack_int flexiblas_chain_LAPACKE_zbbcsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, lapack_int m, lapack_int p, lapack_int q, void *theta, void *phi, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *b11d, void *b11e, void *b12d, void *b12e, void *b21d, void *b21e, void *b22d, void *b22e, void *rwork, lapack_int lrwork);

lapack_int flexiblas_real_LAPACKE_zbdsqr(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zbdsqr(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zbdsqr_work(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_zbdsqr_work(int matrix_layout, char uplo, lapack_int n, lapack_int ncvt, lapack_int nru, lapack_int ncc, void *d, void *e, void *vt, lapack_int ldvt, void *u, lapack_int ldu, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_zcgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *x, lapack_int ldx, void *iter);
lapack_int flexiblas_chain_LAPACKE_zcgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *x, lapack_int ldx, void *iter);

lapack_int flexiblas_real_LAPACKE_zcgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *x, lapack_int ldx, void *work, void *swork, void *rwork, void *iter);
lapack_int flexiblas_chain_LAPACKE_zcgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *x, lapack_int ldx, void *work, void *swork, void *rwork, void *iter);

lapack_int flexiblas_real_LAPACKE_zcposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *x, lapack_int ldx, void *iter);
lapack_int flexiblas_chain_LAPACKE_zcposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *x, lapack_int ldx, void *iter);

lapack_int flexiblas_real_LAPACKE_zcposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *x, lapack_int ldx, void *work, void *swork, void *rwork, void *iter);
lapack_int flexiblas_chain_LAPACKE_zcposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *x, lapack_int ldx, void *work, void *swork, void *rwork, void *iter);

lapack_int flexiblas_real_LAPACKE_zgbbrd(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zgbbrd(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zgbbrd_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgbbrd_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int ncc, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *pt, lapack_int ldpt, void *c, lapack_int ldc, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgbcon(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zgbcon(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zgbcon_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, double anorm, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgbcon_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, const void *ipiv, double anorm, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgbequ(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_zgbequ(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_zgbequ_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_zgbequ_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_zgbequb(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_zgbequb(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_zgbequb_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_zgbequb_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_zgbrfs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zgbrfs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zgbrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgbrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgbsv(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgbsv(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgbsv_work(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgbsv_work(int matrix_layout, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgbsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);
lapack_int flexiblas_chain_LAPACKE_zgbsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);

lapack_int flexiblas_real_LAPACKE_zgbsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgbsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgbtrf(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zgbtrf(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zgbtrf_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zgbtrf_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int kl, lapack_int ku, void *ab, lapack_int ldab, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zgbtrs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgbtrs(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgbtrs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgbtrs_work(int matrix_layout, char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs, const void *ab, lapack_int ldab, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgebak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_zgebak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_zgebak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_zgebak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *scale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_zgebal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);
lapack_int flexiblas_chain_LAPACKE_zgebal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);

lapack_int flexiblas_real_LAPACKE_zgebal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);
lapack_int flexiblas_chain_LAPACKE_zgebal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *ilo, void *ihi, void *scale);

lapack_int flexiblas_real_LAPACKE_zgebrd(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup);
lapack_int flexiblas_chain_LAPACKE_zgebrd(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup);

lapack_int flexiblas_real_LAPACKE_zgebrd_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgebrd_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tauq, void *taup, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgecon(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zgecon(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zgecon_work(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgecon_work(int matrix_layout, char norm, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgedmd_work(int matrix_layout, char jobs, char jobz, char jobr, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *eigs, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *w, lapack_int ldw, void *s, lapack_int lds, void *zwork, lapack_int lzwork, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zgedmd_work(int matrix_layout, char jobs, char jobz, char jobr, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *eigs, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *w, lapack_int ldw, void *s, lapack_int lds, void *zwork, lapack_int lzwork, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zgedmdq_work(int matrix_layout, char jobs, char jobz, char jobr, char jobq, char jobt, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *f, lapack_int ldf, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *eigs, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *v, lapack_int ldv, void *s, lapack_int lds, void *zwork, lapack_int lzwork, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zgedmdq_work(int matrix_layout, char jobs, char jobz, char jobr, char jobq, char jobt, char jobf, lapack_int whtsvd, lapack_int m, lapack_int n, void *f, lapack_int ldf, void *x, lapack_int ldx, void *y, lapack_int ldy, lapack_int nrnk, void *tol, lapack_int k, void *eigs, void *z, lapack_int ldz, void *res, void *b, lapack_int ldb, void *v, lapack_int ldv, void *s, lapack_int lds, void *zwork, lapack_int lzwork, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zgeequ(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_zgeequ(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_zgeequ_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_zgeequ_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_zgeequb(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_zgeequb(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_zgeequb_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);
lapack_int flexiblas_chain_LAPACKE_zgeequb_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *r, void *c, void *rowcnd, void *colcnd, void *amax);

lapack_int flexiblas_real_LAPACKE_zgees(int matrix_layout, char jobvs, char sort, LAPACK_Z_SELECT1 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs);
lapack_int flexiblas_chain_LAPACKE_zgees(int matrix_layout, char jobvs, char sort, LAPACK_Z_SELECT1 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs);

lapack_int flexiblas_real_LAPACKE_zgees_work(int matrix_layout, char jobvs, char sort, LAPACK_Z_SELECT1 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *work, lapack_int lwork, void *rwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_zgees_work(int matrix_layout, char jobvs, char sort, LAPACK_Z_SELECT1 select, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *work, lapack_int lwork, void *rwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_zgeesx(int matrix_layout, char jobvs, char sort, LAPACK_Z_SELECT1 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_zgeesx(int matrix_layout, char jobvs, char sort, LAPACK_Z_SELECT1 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_zgeesx_work(int matrix_layout, char jobvs, char sort, LAPACK_Z_SELECT1 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_zgeesx_work(int matrix_layout, char jobvs, char sort, LAPACK_Z_SELECT1 select, char sense, lapack_int n, void *a, lapack_int lda, void *sdim, void *w, void *vs, lapack_int ldvs, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_zgeev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_zgeev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_zgeev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgeev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgeevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_zgeevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_zgeevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgeevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *scale, void *abnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgehrd(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_zgehrd(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_zgehrd_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgehrd_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgejsv(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *stat, void *istat);
lapack_int flexiblas_chain_LAPACKE_zgejsv(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *stat, void *istat);

lapack_int flexiblas_real_LAPACKE_zgejsv_work(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *cwork, lapack_int lwork, void *work, lapack_int lrwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_zgejsv_work(int matrix_layout, char joba, char jobu, char jobv, char jobr, char jobt, char jobp, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, void *u, lapack_int ldu, void *v, lapack_int ldv, void *cwork, lapack_int lwork, void *work, lapack_int lrwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_zgelq(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);
lapack_int flexiblas_chain_LAPACKE_zgelq(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);

lapack_int flexiblas_real_LAPACKE_zgelq2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_zgelq2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_zgelq2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_zgelq2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_zgelq_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgelq_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgelqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_zgelqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_zgelqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgelqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgels(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgels(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgels_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgels_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgelsd(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_zgelsd(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_zgelsd_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank, void *work, lapack_int lwork, void *rwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_zgelsd_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank, void *work, lapack_int lwork, void *rwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_zgelss(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_zgelss(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_zgelss_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgelss_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *s, double rcond, void *rank, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgelsy(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, double rcond, void *rank);
lapack_int flexiblas_chain_LAPACKE_zgelsy(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, double rcond, void *rank);

lapack_int flexiblas_real_LAPACKE_zgelsy_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, double rcond, void *rank, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgelsy_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *jpvt, double rcond, void *rank, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgemlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zgemlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zgemlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgemlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgemqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zgemqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zgemqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgemqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *t, lapack_int tsize, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgemqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zgemqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zgemqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_zgemqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_zgeqlf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_zgeqlf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_zgeqlf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgeqlf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgeqp3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);
lapack_int flexiblas_chain_LAPACKE_zgeqp3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);

lapack_int flexiblas_real_LAPACKE_zgeqp3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgeqp3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgeqpf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);
lapack_int flexiblas_chain_LAPACKE_zgeqpf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau);

lapack_int flexiblas_real_LAPACKE_zgeqpf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgeqpf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *jpvt, void *tau, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgeqr(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);
lapack_int flexiblas_chain_LAPACKE_zgeqr(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize);

lapack_int flexiblas_real_LAPACKE_zgeqr2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_zgeqr2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_zgeqr2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_zgeqr2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_zgeqr_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgeqr_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int tsize, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgeqrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_zgeqrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_zgeqrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgeqrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgeqrfp(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_zgeqrfp(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_zgeqrfp_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgeqrfp_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgeqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_zgeqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_zgeqrt2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_zgeqrt2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_zgeqrt2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_zgeqrt2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_zgeqrt3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_zgeqrt3(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_zgeqrt3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_zgeqrt3_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_zgeqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *work);
lapack_int flexiblas_chain_LAPACKE_zgeqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *work);

lapack_int flexiblas_real_LAPACKE_zgerfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zgerfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zgerfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgerfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgerqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_zgerqf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_zgerqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgerqf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgesdd(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt);
lapack_int flexiblas_chain_LAPACKE_zgesdd(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt);

lapack_int flexiblas_real_LAPACKE_zgesdd_work(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_zgesdd_work(int matrix_layout, char jobz, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_zgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgesv(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgesv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgesvd(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);
lapack_int flexiblas_chain_LAPACKE_zgesvd(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);

lapack_int flexiblas_real_LAPACKE_zgesvd_work(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgesvd_work(int matrix_layout, char jobu, char jobvt, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgesvdq(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank);
lapack_int flexiblas_chain_LAPACKE_zgesvdq(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank);

lapack_int flexiblas_real_LAPACKE_zgesvdq_work(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank, void *iwork, lapack_int liwork, void *cwork, lapack_int lcwork, void *rwork, lapack_int lrwork);
lapack_int flexiblas_chain_LAPACKE_zgesvdq_work(int matrix_layout, char joba, char jobp, char jobr, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *s, void *u, lapack_int ldu, void *v, lapack_int ldv, void *numrank, void *iwork, lapack_int liwork, void *cwork, lapack_int lcwork, void *rwork, lapack_int lrwork);

lapack_int flexiblas_real_LAPACKE_zgesvdx(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);
lapack_int flexiblas_chain_LAPACKE_zgesvdx(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *superb);

lapack_int flexiblas_real_LAPACKE_zgesvdx_work(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_zgesvdx_work(int matrix_layout, char jobu, char jobvt, char range, lapack_int m, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, void *ns, void *s, void *u, lapack_int ldu, void *vt, lapack_int ldvt, void *work, lapack_int lwork, void *rwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_zgesvj(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *stat);
lapack_int flexiblas_chain_LAPACKE_zgesvj(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *stat);

lapack_int flexiblas_real_LAPACKE_zgesvj_work(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *cwork, lapack_int lwork, void *rwork, lapack_int lrwork);
lapack_int flexiblas_chain_LAPACKE_zgesvj_work(int matrix_layout, char joba, char jobu, char jobv, lapack_int m, lapack_int n, void *a, lapack_int lda, void *sva, lapack_int mv, void *v, lapack_int ldv, void *cwork, lapack_int lwork, void *rwork, lapack_int lrwork);

lapack_int flexiblas_real_LAPACKE_zgesvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);
lapack_int flexiblas_chain_LAPACKE_zgesvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *rpivot);

lapack_int flexiblas_real_LAPACKE_zgesvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgesvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, void *equed, void *r, void *c, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgetf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zgetf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zgetf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zgetf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zgetrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zgetrf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zgetrf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zgetrf2(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zgetrf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zgetrf2_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zgetrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zgetrf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zgetri(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zgetri(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_zgetri_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgetri_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgetrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgetrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgetrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgetrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgetsls(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgetsls(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgetsls_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgetsls_work(int matrix_layout, char trans, lapack_int m, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgetsqrhrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_zgetsqrhrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_zgetsqrhrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgetsqrhrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb1, lapack_int nb1, lapack_int nb2, void *a, lapack_int lda, void *t, lapack_int ldt, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zggbak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_zggbak(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_zggbak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);
lapack_int flexiblas_chain_LAPACKE_zggbak_work(int matrix_layout, char job, char side, lapack_int n, lapack_int ilo, lapack_int ihi, const void *lscale, const void *rscale, lapack_int m, void *v, lapack_int ldv);

lapack_int flexiblas_real_LAPACKE_zggbal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale);
lapack_int flexiblas_chain_LAPACKE_zggbal(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale);

lapack_int flexiblas_real_LAPACKE_zggbal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale, void *work);
lapack_int flexiblas_chain_LAPACKE_zggbal_work(int matrix_layout, char job, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *ilo, void *ihi, void *lscale, void *rscale, void *work);

lapack_int flexiblas_real_LAPACKE_zgges(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);
lapack_int flexiblas_chain_LAPACKE_zgges(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);

lapack_int flexiblas_real_LAPACKE_zgges3(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);
lapack_int flexiblas_chain_LAPACKE_zgges3(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr);

lapack_int flexiblas_real_LAPACKE_zgges3_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *rwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_zgges3_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *rwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_zgges_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *rwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_zgges_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *work, lapack_int lwork, void *rwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_zggesx(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_zggesx(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_zggesx_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *iwork, lapack_int liwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_zggesx_work(int matrix_layout, char jobvsl, char jobvsr, char sort, LAPACK_Z_SELECT2 selctg, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *sdim, void *alpha, void *beta, void *vsl, lapack_int ldvsl, void *vsr, lapack_int ldvsr, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *iwork, lapack_int liwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_zggev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_zggev(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_zggev3(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);
lapack_int flexiblas_chain_LAPACKE_zggev3(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr);

lapack_int flexiblas_real_LAPACKE_zggev3_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zggev3_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zggev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zggev_work(int matrix_layout, char jobvl, char jobvr, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zggevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv);
lapack_int flexiblas_chain_LAPACKE_zggevx(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv);

lapack_int flexiblas_real_LAPACKE_zggevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *iwork, void *bwork);
lapack_int flexiblas_chain_LAPACKE_zggevx_work(int matrix_layout, char balanc, char jobvl, char jobvr, char sense, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, void *ilo, void *ihi, void *lscale, void *rscale, void *abnrm, void *bbnrm, void *rconde, void *rcondv, void *work, lapack_int lwork, void *rwork, void *iwork, void *bwork);

lapack_int flexiblas_real_LAPACKE_zggglm(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y);
lapack_int flexiblas_chain_LAPACKE_zggglm(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y);

lapack_int flexiblas_real_LAPACKE_zggglm_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zggglm_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *d, void *x, void *y, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgghd3(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zgghd3(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zgghd3_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgghd3_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zgghrd(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zgghrd(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zgghrd_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zgghrd_work(int matrix_layout, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zgglse(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x);
lapack_int flexiblas_chain_LAPACKE_zgglse(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x);

lapack_int flexiblas_real_LAPACKE_zgglse_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zgglse_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int p, void *a, lapack_int lda, void *b, lapack_int ldb, void *c, void *d, void *x, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zggqrf(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);
lapack_int flexiblas_chain_LAPACKE_zggqrf(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);

lapack_int flexiblas_real_LAPACKE_zggqrf_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zggqrf_work(int matrix_layout, lapack_int n, lapack_int m, lapack_int p, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zggrqf(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);
lapack_int flexiblas_chain_LAPACKE_zggrqf(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub);

lapack_int flexiblas_real_LAPACKE_zggrqf_work(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zggrqf_work(int matrix_layout, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *taua, void *b, lapack_int ldb, void *taub, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zggsvd(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);
lapack_int flexiblas_chain_LAPACKE_zggsvd(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);

lapack_int flexiblas_real_LAPACKE_zggsvd3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);
lapack_int flexiblas_chain_LAPACKE_zggsvd3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork);

lapack_int flexiblas_real_LAPACKE_zggsvd3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, lapack_int lwork, void *rwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_zggsvd3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, lapack_int lwork, void *rwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_zggsvd_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *rwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_zggsvd_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int n, lapack_int p, void *k, void *l, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *rwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_zggsvp(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_zggsvp(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_zggsvp3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_zggsvp3(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_zggsvp3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *rwork, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zggsvp3_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *rwork, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zggsvp_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *rwork, void *tau, void *work);
lapack_int flexiblas_chain_LAPACKE_zggsvp_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *k, void *l, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *iwork, void *rwork, void *tau, void *work);

lapack_int flexiblas_real_LAPACKE_zgtcon(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zgtcon(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zgtcon_work(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, double anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_zgtcon_work(char norm, lapack_int n, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, double anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_zgtrfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zgtrfs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zgtrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgtrfs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *dlf, const void *df, const void *duf, const void *du2, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgtsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgtsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgtsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgtsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *dl, void *d, void *du, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgtsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zgtsvx(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zgtsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zgtsvx_work(int matrix_layout, char fact, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, void *dlf, void *df, void *duf, void *du2, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zgttrf(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zgttrf(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zgttrf_work(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zgttrf_work(lapack_int n, void *dl, void *d, void *du, void *du2, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zgttrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgttrs(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zgttrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zgttrs_work(int matrix_layout, char trans, lapack_int n, lapack_int nrhs, const void *dl, const void *d, const void *du, const void *du2, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhbev(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhbev(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhbev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhbev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhbev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhbev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhbev_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhbev_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhbevd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhbevd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhbevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhbevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhbevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zhbevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zhbevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zhbevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zhbevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhbevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhbevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhbevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhbevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhbevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhbevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhbevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhbgst(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx);
lapack_int flexiblas_chain_LAPACKE_zhbgst(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx);

lapack_int flexiblas_real_LAPACKE_zhbgst_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhbgst_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, const void *bb, lapack_int ldbb, void *x, lapack_int ldx, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhbgv(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhbgv(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhbgv_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhbgv_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhbgvd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhbgvd(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhbgvd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zhbgvd_work(int matrix_layout, char jobz, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zhbgvx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhbgvx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhbgvx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhbgvx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, lapack_int ka, lapack_int kb, void *ab, lapack_int ldab, void *bb, lapack_int ldbb, void *q, lapack_int ldq, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhbtrd(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_zhbtrd(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_zhbtrd_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *work);
lapack_int flexiblas_chain_LAPACKE_zhbtrd_work(int matrix_layout, char vect, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab, void *d, void *e, void *q, lapack_int ldq, void *work);

lapack_int flexiblas_real_LAPACKE_zhecon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zhecon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zhecon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zhecon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zhecon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_zhecon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_zhecon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_zhecon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_zheequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_zheequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_zheequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);
lapack_int flexiblas_chain_LAPACKE_zheequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);

lapack_int flexiblas_real_LAPACKE_zheev(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_zheev(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_zheev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_zheev_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_zheev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zheev_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zheev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zheev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zheevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_zheevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_zheevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);
lapack_int flexiblas_chain_LAPACKE_zheevd_2stage(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w);

lapack_int flexiblas_real_LAPACKE_zheevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zheevd_2stage_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zheevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zheevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zheevr(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_zheevr(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_zheevr_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_zheevr_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_zheevr_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zheevr_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zheevr_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zheevr_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zheevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zheevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_zheevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zheevx_2stage(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_zheevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zheevx_2stage_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_zheevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zheevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhegst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhegst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhegst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhegst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *a, lapack_int lda, const void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhegv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_zhegv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_zhegv_2stage(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_zhegv_2stage(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_zhegv_2stage_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhegv_2stage_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhegv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhegv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhegvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);
lapack_int flexiblas_chain_LAPACKE_zhegvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w);

lapack_int flexiblas_real_LAPACKE_zhegvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zhegvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *w, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zhegvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhegvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhegvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhegvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_zherfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zherfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zherfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zherfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhesv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhesv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhesv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhesv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhesv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhesv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhesv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhesv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhesv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhesv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhesv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhesv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhesv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhesv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhesv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhesv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhesvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zhesvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zhesvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhesvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zheswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_zheswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_zheswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_zheswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_zhetrd(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_zhetrd(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_zhetrd_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhetrd_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *d, void *e, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhetrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zhetrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zhetrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zhetrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zhetrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);
lapack_int flexiblas_chain_LAPACKE_zhetrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);

lapack_int flexiblas_real_LAPACKE_zhetrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhetrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhetrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhetrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhetrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zhetrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zhetrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhetrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhetrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zhetrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zhetrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhetrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhetrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhetrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhetri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zhetri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_zhetri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zhetri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_zhetri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhetri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhetri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_zhetri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_zhetri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_zhetri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_zhetri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zhetri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_zhetri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhetri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhetri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_zhetri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_zhetrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhetrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhetrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhetrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhetrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);
lapack_int flexiblas_chain_LAPACKE_zhetrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);

lapack_int flexiblas_real_LAPACKE_zhetrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhetrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhetrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhetrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhetrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhetrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhetrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhetrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhetrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhetrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhetrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhetrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zhetrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhetrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhetrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhetrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhetrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhetrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhfrk(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, double alpha, const void *a, lapack_int lda, double beta, void *c);
lapack_int flexiblas_chain_LAPACKE_zhfrk(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, double alpha, const void *a, lapack_int lda, double beta, void *c);

lapack_int flexiblas_real_LAPACKE_zhfrk_work(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, double alpha, const void *a, lapack_int lda, double beta, void *c);
lapack_int flexiblas_chain_LAPACKE_zhfrk_work(int matrix_layout, char transr, char uplo, char trans, lapack_int n, lapack_int k, double alpha, const void *a, lapack_int lda, double beta, void *c);

lapack_int flexiblas_real_LAPACKE_zhgeqz(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhgeqz(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhgeqz_work(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhgeqz_work(int matrix_layout, char job, char compq, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *t, lapack_int ldt, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhpcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zhpcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zhpcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_zhpcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_zhpev(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhpev(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhpev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhpev_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhpevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhpevd(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhpevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zhpevd_work(int matrix_layout, char jobz, char uplo, lapack_int n, void *ap, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zhpevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhpevx(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhpevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhpevx_work(int matrix_layout, char jobz, char range, char uplo, lapack_int n, void *ap, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhpgst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);
lapack_int flexiblas_chain_LAPACKE_zhpgst(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);

lapack_int flexiblas_real_LAPACKE_zhpgst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);
lapack_int flexiblas_chain_LAPACKE_zhpgst_work(int matrix_layout, lapack_int itype, char uplo, lapack_int n, void *ap, const void *bp);

lapack_int flexiblas_real_LAPACKE_zhpgv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhpgv(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhpgv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhpgv_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhpgvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhpgvd(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhpgvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zhpgvd_work(int matrix_layout, lapack_int itype, char jobz, char uplo, lapack_int n, void *ap, void *bp, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zhpgvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhpgvx(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhpgvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);
lapack_int flexiblas_chain_LAPACKE_zhpgvx_work(int matrix_layout, lapack_int itype, char jobz, char range, char uplo, lapack_int n, void *ap, void *bp, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *work, void *rwork, void *iwork, void *ifail);

lapack_int flexiblas_real_LAPACKE_zhprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zhprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zhprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhpsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhpsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhpsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhpsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhpsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zhpsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zhpsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zhpsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zhptrd(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_zhptrd(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_zhptrd_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);
lapack_int flexiblas_chain_LAPACKE_zhptrd_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *d, void *e, void *tau);

lapack_int flexiblas_real_LAPACKE_zhptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zhptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zhptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zhptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zhptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zhptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_zhptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_zhptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_zhptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zhptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zhsein(int matrix_layout, char job, char eigsrc, char initv, const void *select, lapack_int n, const void *h, lapack_int ldh, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *ifaill, void *ifailr);
lapack_int flexiblas_chain_LAPACKE_zhsein(int matrix_layout, char job, char eigsrc, char initv, const void *select, lapack_int n, const void *h, lapack_int ldh, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *ifaill, void *ifailr);

lapack_int flexiblas_real_LAPACKE_zhsein_work(int matrix_layout, char job, char eigsrc, char initv, const void *select, lapack_int n, const void *h, lapack_int ldh, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork, void *ifaill, void *ifailr);
lapack_int flexiblas_chain_LAPACKE_zhsein_work(int matrix_layout, char job, char eigsrc, char initv, const void *select, lapack_int n, const void *h, lapack_int ldh, void *w, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork, void *ifaill, void *ifailr);

lapack_int flexiblas_real_LAPACKE_zhseqr(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *w, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zhseqr(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *w, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zhseqr_work(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zhseqr_work(int matrix_layout, char job, char compz, lapack_int n, lapack_int ilo, lapack_int ihi, void *h, lapack_int ldh, void *w, void *z, lapack_int ldz, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zlacgv(lapack_int n, void *x, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_zlacgv(lapack_int n, void *x, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_zlacgv_work(lapack_int n, void *x, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_zlacgv_work(lapack_int n, void *x, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_zlacn2(lapack_int n, void *v, void *x, void *est, void *kase, void *isave);
lapack_int flexiblas_chain_LAPACKE_zlacn2(lapack_int n, void *v, void *x, void *est, void *kase, void *isave);

lapack_int flexiblas_real_LAPACKE_zlacn2_work(lapack_int n, void *v, void *x, void *est, void *kase, void *isave);
lapack_int flexiblas_chain_LAPACKE_zlacn2_work(lapack_int n, void *v, void *x, void *est, void *kase, void *isave);

lapack_int flexiblas_real_LAPACKE_zlacp2(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zlacp2(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zlacp2_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zlacp2_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zlacpy(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zlacpy(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zlacpy_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zlacpy_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zlacrm(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zlacrm(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zlacrm_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_zlacrm_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_zlag2c(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *sa, lapack_int ldsa);
lapack_int flexiblas_chain_LAPACKE_zlag2c(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *sa, lapack_int ldsa);

lapack_int flexiblas_real_LAPACKE_zlag2c_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *sa, lapack_int ldsa);
lapack_int flexiblas_chain_LAPACKE_zlag2c_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *sa, lapack_int ldsa);

double flexiblas_real_LAPACKE_zlangb(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab);
double flexiblas_chain_LAPACKE_zlangb(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab);

double flexiblas_real_LAPACKE_zlangb_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *work);
double flexiblas_chain_LAPACKE_zlangb_work(int matrix_layout, char norm, lapack_int n, lapack_int kl, lapack_int ku, const void *ab, lapack_int ldab, void *work);

double flexiblas_real_LAPACKE_zlange(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda);
double flexiblas_chain_LAPACKE_zlange(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda);

double flexiblas_real_LAPACKE_zlange_work(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);
double flexiblas_chain_LAPACKE_zlange_work(int matrix_layout, char norm, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);

double flexiblas_real_LAPACKE_zlanhe(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);
double flexiblas_chain_LAPACKE_zlanhe(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);

double flexiblas_real_LAPACKE_zlanhe_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);
double flexiblas_chain_LAPACKE_zlanhe_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);

double flexiblas_real_LAPACKE_zlansy(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);
double flexiblas_chain_LAPACKE_zlansy(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda);

double flexiblas_real_LAPACKE_zlansy_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);
double flexiblas_chain_LAPACKE_zlansy_work(int matrix_layout, char norm, char uplo, lapack_int n, const void *a, lapack_int lda, void *work);

double flexiblas_real_LAPACKE_zlantr(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda);
double flexiblas_chain_LAPACKE_zlantr(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda);

double flexiblas_real_LAPACKE_zlantr_work(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);
double flexiblas_chain_LAPACKE_zlantr_work(int matrix_layout, char norm, char uplo, char diag, lapack_int m, lapack_int n, const void *a, lapack_int lda, void *work);

lapack_int flexiblas_real_LAPACKE_zlapmr(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_zlapmr(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_zlapmr_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_zlapmr_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_zlapmt(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_zlapmt(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_zlapmt_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);
lapack_int flexiblas_chain_LAPACKE_zlapmt_work(int matrix_layout, lapack_int forwrd, lapack_int m, lapack_int n, void *x, lapack_int ldx, void *k);

lapack_int flexiblas_real_LAPACKE_zlarcm(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zlarcm(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zlarcm_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_zlarcm_work(int matrix_layout, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_zlarfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zlarfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zlarfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work, lapack_int ldwork);
lapack_int flexiblas_chain_LAPACKE_zlarfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *c, lapack_int ldc, void *work, lapack_int ldwork);

lapack_int flexiblas_real_LAPACKE_zlarfg(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);
lapack_int flexiblas_chain_LAPACKE_zlarfg(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);

lapack_int flexiblas_real_LAPACKE_zlarfg_work(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);
lapack_int flexiblas_chain_LAPACKE_zlarfg_work(lapack_int n, void *alpha, void *x, lapack_int incx, void *tau);

lapack_int flexiblas_real_LAPACKE_zlarft(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_zlarft(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_zlarft_work(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_zlarft_work(int matrix_layout, char direct, char storev, lapack_int n, lapack_int k, const void *v, lapack_int ldv, const void *tau, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_zlarfx(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, double _Complex tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_zlarfx(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, double _Complex tau, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_zlarfx_work(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, double _Complex tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_zlarfx_work(int matrix_layout, char side, lapack_int m, lapack_int n, const void *v, double _Complex tau, void *c, lapack_int ldc, void *work);

lapack_int flexiblas_real_LAPACKE_zlarnv(lapack_int idist, void *iseed, lapack_int n, void *x);
lapack_int flexiblas_chain_LAPACKE_zlarnv(lapack_int idist, void *iseed, lapack_int n, void *x);

lapack_int flexiblas_real_LAPACKE_zlarnv_work(lapack_int idist, void *iseed, lapack_int n, void *x);
lapack_int flexiblas_chain_LAPACKE_zlarnv_work(lapack_int idist, void *iseed, lapack_int n, void *x);

lapack_int flexiblas_real_LAPACKE_zlascl(int matrix_layout, char type, lapack_int kl, lapack_int ku, double cfrom, double cto, lapack_int m, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zlascl(int matrix_layout, char type, lapack_int kl, lapack_int ku, double cfrom, double cto, lapack_int m, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zlascl_work(int matrix_layout, char type, lapack_int kl, lapack_int ku, double cfrom, double cto, lapack_int m, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zlascl_work(int matrix_layout, char type, lapack_int kl, lapack_int ku, double cfrom, double cto, lapack_int m, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zlaset(int matrix_layout, char uplo, lapack_int m, lapack_int n, double _Complex alpha, double _Complex beta, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zlaset(int matrix_layout, char uplo, lapack_int m, lapack_int n, double _Complex alpha, double _Complex beta, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zlaset_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, double _Complex alpha, double _Complex beta, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zlaset_work(int matrix_layout, char uplo, lapack_int m, lapack_int n, double _Complex alpha, double _Complex beta, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zlassq(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);
lapack_int flexiblas_chain_LAPACKE_zlassq(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);

lapack_int flexiblas_real_LAPACKE_zlassq_work(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);
lapack_int flexiblas_chain_LAPACKE_zlassq_work(lapack_int n, void *x, lapack_int incx, void *scale, void *sumsq);

lapack_int flexiblas_real_LAPACKE_zlaswp(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_zlaswp(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_zlaswp_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);
lapack_int flexiblas_chain_LAPACKE_zlaswp_work(int matrix_layout, lapack_int n, void *a, lapack_int lda, lapack_int k1, lapack_int k2, const void *ipiv, lapack_int incx);

lapack_int flexiblas_real_LAPACKE_zlauum(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zlauum(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zlauum_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zlauum_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zpbcon(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zpbcon(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zpbcon_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, double anorm, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zpbcon_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, double anorm, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zpbequ(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_zpbequ(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_zpbequ_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_zpbequ_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_zpbrfs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zpbrfs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zpbrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zpbrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *afb, lapack_int ldafb, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zpbstf(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);
lapack_int flexiblas_chain_LAPACKE_zpbstf(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);

lapack_int flexiblas_real_LAPACKE_zpbstf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);
lapack_int flexiblas_chain_LAPACKE_zpbstf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kb, void *bb, lapack_int ldbb);

lapack_int flexiblas_real_LAPACKE_zpbsv(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpbsv(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zpbsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpbsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zpbsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zpbsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zpbsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zpbsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, void *ab, lapack_int ldab, void *afb, lapack_int ldafb, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zpbtrf(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);
lapack_int flexiblas_chain_LAPACKE_zpbtrf(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);

lapack_int flexiblas_real_LAPACKE_zpbtrf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);
lapack_int flexiblas_chain_LAPACKE_zpbtrf_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, void *ab, lapack_int ldab);

lapack_int flexiblas_real_LAPACKE_zpbtrs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpbtrs(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zpbtrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpbtrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zpftrf(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_zpftrf(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_zpftrf_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_zpftrf_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_zpftri(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_zpftri(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_zpftri_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_zpftri_work(int matrix_layout, char transr, char uplo, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_zpftrs(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpftrs(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zpftrs_work(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpftrs_work(int matrix_layout, char transr, char uplo, lapack_int n, lapack_int nrhs, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zpocon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zpocon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zpocon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zpocon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, double anorm, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zpoequ(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_zpoequ(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_zpoequ_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_zpoequ_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_zpoequb(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_zpoequb(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_zpoequb_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_zpoequb_work(int matrix_layout, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_zporfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zporfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zporfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zporfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zposv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zposv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zposvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zposvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zposvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zposvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *af, lapack_int ldaf, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zpotrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zpotrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zpotrf2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zpotrf2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zpotrf2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zpotrf2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zpotrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zpotrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zpotri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zpotri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zpotri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zpotri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zpotrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpotrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zpotrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpotrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zppcon(int matrix_layout, char uplo, lapack_int n, const void *ap, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zppcon(int matrix_layout, char uplo, lapack_int n, const void *ap, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zppcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, double anorm, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zppcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, double anorm, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zppequ(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_zppequ(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_zppequ_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_zppequ_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_zpprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zpprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zpprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zpprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zppsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zppsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zppsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zppsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zppsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zppsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zppsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zppsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *afp, void *equed, void *s, void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zpptrf(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_zpptrf(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_zpptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_zpptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_zpptri(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_zpptri(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_zpptri_work(int matrix_layout, char uplo, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_zpptri_work(int matrix_layout, char uplo, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_zpptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zpptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zpstrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, double tol);
lapack_int flexiblas_chain_LAPACKE_zpstrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, double tol);

lapack_int flexiblas_real_LAPACKE_zpstrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, double tol, void *work);
lapack_int flexiblas_chain_LAPACKE_zpstrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *piv, void *rank, double tol, void *work);

lapack_int flexiblas_real_LAPACKE_zptcon(lapack_int n, const void *d, const void *e, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zptcon(lapack_int n, const void *d, const void *e, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zptcon_work(lapack_int n, const void *d, const void *e, double anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_zptcon_work(lapack_int n, const void *d, const void *e, double anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_zpteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zpteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zpteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_zpteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_zptrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zptrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zptrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zptrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, const void *df, const void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zptsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zptsv(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zptsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zptsv_work(int matrix_layout, lapack_int n, lapack_int nrhs, void *d, void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zptsvx(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zptsvx(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zptsvx_work(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zptsvx_work(int matrix_layout, char fact, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *df, void *ef, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zpttrf(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_zpttrf(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_zpttrf_work(lapack_int n, void *d, void *e);
lapack_int flexiblas_chain_LAPACKE_zpttrf_work(lapack_int n, void *d, void *e);

lapack_int flexiblas_real_LAPACKE_zpttrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpttrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zpttrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zpttrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *d, const void *e, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zspcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zspcon(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zspcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_zspcon_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *ipiv, double anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_zsprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zsprfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zsprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zsprfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *afp, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zspsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zspsv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zspsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zspsv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *ap, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zspsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zspsvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zspsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zspsvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *ap, void *afp, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zsptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zsptrf(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zsptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zsptrf_work(int matrix_layout, char uplo, lapack_int n, void *ap, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zsptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zsptri(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_zsptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_zsptri_work(int matrix_layout, char uplo, lapack_int n, void *ap, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_zsptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsptrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsptrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *ap, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zstedc(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zstedc(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zstedc_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zstedc_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zstegr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);
lapack_int flexiblas_chain_LAPACKE_zstegr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz);

lapack_int flexiblas_real_LAPACKE_zstegr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zstegr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, double abstol, void *m, void *w, void *z, lapack_int ldz, void *isuppz, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zstein(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *ifailv);
lapack_int flexiblas_chain_LAPACKE_zstein(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *ifailv);

lapack_int flexiblas_real_LAPACKE_zstein_work(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *work, void *iwork, void *ifailv);
lapack_int flexiblas_chain_LAPACKE_zstein_work(int matrix_layout, lapack_int n, const void *d, const void *e, lapack_int m, const void *w, const void *iblock, const void *isplit, void *z, lapack_int ldz, void *work, void *iwork, void *ifailv);

lapack_int flexiblas_real_LAPACKE_zstemr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac);
lapack_int flexiblas_chain_LAPACKE_zstemr(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac);

lapack_int flexiblas_real_LAPACKE_zstemr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_zstemr_work(int matrix_layout, char jobz, char range, lapack_int n, void *d, void *e, double vl, double vu, lapack_int il, lapack_int iu, void *m, void *w, void *z, lapack_int ldz, lapack_int nzc, void *isuppz, void *tryrac, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_zsteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);
lapack_int flexiblas_chain_LAPACKE_zsteqr(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz);

lapack_int flexiblas_real_LAPACKE_zsteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);
lapack_int flexiblas_chain_LAPACKE_zsteqr_work(int matrix_layout, char compz, lapack_int n, void *d, void *e, void *z, lapack_int ldz, void *work);

lapack_int flexiblas_real_LAPACKE_zsycon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zsycon(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zsycon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond);
lapack_int flexiblas_chain_LAPACKE_zsycon_3(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond);

lapack_int flexiblas_real_LAPACKE_zsycon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_zsycon_3_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *e, const void *ipiv, double anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_zsycon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond, void *work);
lapack_int flexiblas_chain_LAPACKE_zsycon_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, const void *ipiv, double anorm, void *rcond, void *work);

lapack_int flexiblas_real_LAPACKE_zsyconv(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);
lapack_int flexiblas_chain_LAPACKE_zsyconv(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);

lapack_int flexiblas_real_LAPACKE_zsyconv_work(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);
lapack_int flexiblas_chain_LAPACKE_zsyconv_work(int matrix_layout, char uplo, char way, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *e);

lapack_int flexiblas_real_LAPACKE_zsyequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);
lapack_int flexiblas_chain_LAPACKE_zsyequb(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax);

lapack_int flexiblas_real_LAPACKE_zsyequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);
lapack_int flexiblas_chain_LAPACKE_zsyequb_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *s, void *scond, void *amax, void *work);

lapack_int flexiblas_real_LAPACKE_zsyr(int matrix_layout, char uplo, lapack_int n, double _Complex alpha, const void *x, lapack_int incx, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zsyr(int matrix_layout, char uplo, lapack_int n, double _Complex alpha, const void *x, lapack_int incx, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zsyr_work(int matrix_layout, char uplo, lapack_int n, double _Complex alpha, const void *x, lapack_int incx, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_zsyr_work(int matrix_layout, char uplo, lapack_int n, double _Complex alpha, const void *x, lapack_int incx, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_zsyrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zsyrfs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zsyrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zsyrfs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *af, lapack_int ldaf, const void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_zsysv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsysv(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsysv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsysv_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsysv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsysv_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsysv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsysv_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsysv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsysv_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsysv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsysv_rk(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsysv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsysv_rk_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *e, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsysv_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsysv_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsysv_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsysv_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsysv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsysv_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsysvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_zsysvx(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_zsysvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_zsysvx_work(int matrix_layout, char fact, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *af, lapack_int ldaf, void *ipiv, const void *b, lapack_int ldb, void *x, lapack_int ldx, void *rcond, void *ferr, void *berr, void *work, lapack_int lwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_zsyswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_zsyswapr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_zsyswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);
lapack_int flexiblas_chain_LAPACKE_zsyswapr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, lapack_int i1, lapack_int i2);

lapack_int flexiblas_real_LAPACKE_zsytrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zsytrf(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zsytrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zsytrf_aa(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zsytrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);
lapack_int flexiblas_chain_LAPACKE_zsytrf_aa_2stage(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2);

lapack_int flexiblas_real_LAPACKE_zsytrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsytrf_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsytrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsytrf_aa_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsytrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zsytrf_rk(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zsytrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsytrf_rk_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *e, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsytrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zsytrf_rook(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv);

lapack_int flexiblas_real_LAPACKE_zsytrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsytrf_rook_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsytrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsytrf_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsytri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zsytri(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_zsytri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zsytri2(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_zsytri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsytri2_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsytri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_zsytri2x(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_zsytri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);
lapack_int flexiblas_chain_LAPACKE_zsytri2x_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work, lapack_int nb);

lapack_int flexiblas_real_LAPACKE_zsytri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);
lapack_int flexiblas_chain_LAPACKE_zsytri_3(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv);

lapack_int flexiblas_real_LAPACKE_zsytri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsytri_3_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *e, const void *ipiv, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsytri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);
lapack_int flexiblas_chain_LAPACKE_zsytri_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *ipiv, void *work);

lapack_int flexiblas_real_LAPACKE_zsytrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsytrs(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsytrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsytrs2(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsytrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);
lapack_int flexiblas_chain_LAPACKE_zsytrs2_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work);

lapack_int flexiblas_real_LAPACKE_zsytrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsytrs_3(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsytrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsytrs_3_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *e, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsytrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsytrs_aa(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsytrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsytrs_aa_2stage(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsytrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsytrs_aa_2stage_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, void *a, lapack_int lda, void *tb, lapack_int ltb, void *ipiv, void *ipiv2, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsytrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zsytrs_aa_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zsytrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsytrs_rook(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsytrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsytrs_rook_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_zsytrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_zsytrs_work(int matrix_layout, char uplo, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *ipiv, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ztbcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond);
lapack_int flexiblas_chain_LAPACKE_ztbcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond);

lapack_int flexiblas_real_LAPACKE_ztbcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ztbcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, lapack_int kd, const void *ab, lapack_int ldab, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ztbrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_ztbrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_ztbrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ztbrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ztbtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ztbtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ztbtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ztbtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int kd, lapack_int nrhs, const void *ab, lapack_int ldab, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ztfsm(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, double _Complex alpha, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ztfsm(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, double _Complex alpha, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ztfsm_work(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, double _Complex alpha, const void *a, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ztfsm_work(int matrix_layout, char transr, char side, char uplo, char trans, char diag, lapack_int m, lapack_int n, double _Complex alpha, const void *a, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ztftri(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_ztftri(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_ztftri_work(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);
lapack_int flexiblas_chain_LAPACKE_ztftri_work(int matrix_layout, char transr, char uplo, char diag, lapack_int n, void *a);

lapack_int flexiblas_real_LAPACKE_ztfttp(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);
lapack_int flexiblas_chain_LAPACKE_ztfttp(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);

lapack_int flexiblas_real_LAPACKE_ztfttp_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);
lapack_int flexiblas_chain_LAPACKE_ztfttp_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *ap);

lapack_int flexiblas_real_LAPACKE_ztfttr(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ztfttr(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ztfttr_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ztfttr_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *arf, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ztgevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_ztgevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_ztgevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ztgevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, const void *s, lapack_int lds, const void *p, lapack_int ldp, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ztgexc(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, lapack_int ifst, lapack_int ilst);
lapack_int flexiblas_chain_LAPACKE_ztgexc(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, lapack_int ifst, lapack_int ilst);

lapack_int flexiblas_real_LAPACKE_ztgexc_work(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, lapack_int ifst, lapack_int ilst);
lapack_int flexiblas_chain_LAPACKE_ztgexc_work(int matrix_layout, lapack_int wantq, lapack_int wantz, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *q, lapack_int ldq, void *z, lapack_int ldz, lapack_int ifst, lapack_int ilst);

lapack_int flexiblas_real_LAPACKE_ztgsen(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif);
lapack_int flexiblas_chain_LAPACKE_ztgsen(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif);

lapack_int flexiblas_real_LAPACKE_ztgsen_work(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif, void *work, lapack_int lwork, void *iwork, lapack_int liwork);
lapack_int flexiblas_chain_LAPACKE_ztgsen_work(int matrix_layout, lapack_int ijob, lapack_int wantq, lapack_int wantz, const void *select, lapack_int n, void *a, lapack_int lda, void *b, lapack_int ldb, void *alpha, void *beta, void *q, lapack_int ldq, void *z, lapack_int ldz, void *m, void *pl, void *pr, void *dif, void *work, lapack_int lwork, void *iwork, lapack_int liwork);

lapack_int flexiblas_real_LAPACKE_ztgsja(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *ncycle);
lapack_int flexiblas_chain_LAPACKE_ztgsja(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *ncycle);

lapack_int flexiblas_real_LAPACKE_ztgsja_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *ncycle);
lapack_int flexiblas_chain_LAPACKE_ztgsja_work(int matrix_layout, char jobu, char jobv, char jobq, lapack_int m, lapack_int p, lapack_int n, lapack_int k, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, double tola, double tolb, void *alpha, void *beta, void *u, lapack_int ldu, void *v, lapack_int ldv, void *q, lapack_int ldq, void *work, void *ncycle);

lapack_int flexiblas_real_LAPACKE_ztgsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_ztgsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_ztgsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_ztgsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *dif, lapack_int mm, void *m, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_ztgsyl(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif);
lapack_int flexiblas_chain_LAPACKE_ztgsyl(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif);

lapack_int flexiblas_real_LAPACKE_ztgsyl_work(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif, void *work, lapack_int lwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_ztgsyl_work(int matrix_layout, char trans, lapack_int ijob, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, const void *d, lapack_int ldd, const void *e, lapack_int lde, void *f, lapack_int ldf, void *scale, void *dif, void *work, lapack_int lwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_ztpcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond);
lapack_int flexiblas_chain_LAPACKE_ztpcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond);

lapack_int flexiblas_real_LAPACKE_ztpcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ztpcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *ap, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ztpmqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ztpmqrt(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ztpmqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work);
lapack_int flexiblas_chain_LAPACKE_ztpmqrt_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, lapack_int nb, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work);

lapack_int flexiblas_real_LAPACKE_ztpqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_ztpqrt(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_ztpqrt2(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_ztpqrt2(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_ztpqrt2_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_ztpqrt2_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_ztpqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt, void *work);
lapack_int flexiblas_chain_LAPACKE_ztpqrt_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int l, lapack_int nb, void *a, lapack_int lda, void *b, lapack_int ldb, void *t, lapack_int ldt, void *work);

lapack_int flexiblas_real_LAPACKE_ztprfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ztprfb(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ztprfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int ldwork);
lapack_int flexiblas_chain_LAPACKE_ztprfb_work(int matrix_layout, char side, char trans, char direct, char storev, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *v, lapack_int ldv, const void *t, lapack_int ldt, void *a, lapack_int lda, void *b, lapack_int ldb, void *work, lapack_int ldwork);

lapack_int flexiblas_real_LAPACKE_ztprfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_ztprfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_ztprfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ztprfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ztptri(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_ztptri(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_ztptri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);
lapack_int flexiblas_chain_LAPACKE_ztptri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *ap);

lapack_int flexiblas_real_LAPACKE_ztptrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ztptrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ztptrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ztptrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *ap, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ztpttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);
lapack_int flexiblas_chain_LAPACKE_ztpttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);

lapack_int flexiblas_real_LAPACKE_ztpttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);
lapack_int flexiblas_chain_LAPACKE_ztpttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *ap, void *arf);

lapack_int flexiblas_real_LAPACKE_ztpttr(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ztpttr(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ztpttr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ztpttr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ztrcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond);
lapack_int flexiblas_chain_LAPACKE_ztrcon(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond);

lapack_int flexiblas_real_LAPACKE_ztrcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ztrcon_work(int matrix_layout, char norm, char uplo, char diag, lapack_int n, const void *a, lapack_int lda, void *rcond, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ztrevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_ztrevc(int matrix_layout, char side, char howmny, const void *select, lapack_int n, void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_ztrevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ztrevc_work(int matrix_layout, char side, char howmny, const void *select, lapack_int n, void *t, lapack_int ldt, void *vl, lapack_int ldvl, void *vr, lapack_int ldvr, lapack_int mm, void *m, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ztrexc(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, lapack_int ifst, lapack_int ilst);
lapack_int flexiblas_chain_LAPACKE_ztrexc(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, lapack_int ifst, lapack_int ilst);

lapack_int flexiblas_real_LAPACKE_ztrexc_work(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, lapack_int ifst, lapack_int ilst);
lapack_int flexiblas_chain_LAPACKE_ztrexc_work(int matrix_layout, char compq, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, lapack_int ifst, lapack_int ilst);

lapack_int flexiblas_real_LAPACKE_ztrrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);
lapack_int flexiblas_chain_LAPACKE_ztrrfs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr);

lapack_int flexiblas_real_LAPACKE_ztrrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ztrrfs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, const void *b, lapack_int ldb, const void *x, lapack_int ldx, void *ferr, void *berr, void *work, void *rwork);

lapack_int flexiblas_real_LAPACKE_ztrsen(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *w, void *m, void *s, void *sep);
lapack_int flexiblas_chain_LAPACKE_ztrsen(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *w, void *m, void *s, void *sep);

lapack_int flexiblas_real_LAPACKE_ztrsen_work(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *w, void *m, void *s, void *sep, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ztrsen_work(int matrix_layout, char job, char compq, const void *select, lapack_int n, void *t, lapack_int ldt, void *q, lapack_int ldq, void *w, void *m, void *s, void *sep, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_ztrsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m);
lapack_int flexiblas_chain_LAPACKE_ztrsna(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m);

lapack_int flexiblas_real_LAPACKE_ztrsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m, void *work, lapack_int ldwork, void *rwork);
lapack_int flexiblas_chain_LAPACKE_ztrsna_work(int matrix_layout, char job, char howmny, const void *select, lapack_int n, const void *t, lapack_int ldt, const void *vl, lapack_int ldvl, const void *vr, lapack_int ldvr, void *s, void *sep, lapack_int mm, void *m, void *work, lapack_int ldwork, void *rwork);

lapack_int flexiblas_real_LAPACKE_ztrsyl(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);
lapack_int flexiblas_chain_LAPACKE_ztrsyl(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);

lapack_int flexiblas_real_LAPACKE_ztrsyl3(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);
lapack_int flexiblas_chain_LAPACKE_ztrsyl3(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);

lapack_int flexiblas_real_LAPACKE_ztrsyl3_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale, void *swork, lapack_int ldswork);
lapack_int flexiblas_chain_LAPACKE_ztrsyl3_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale, void *swork, lapack_int ldswork);

lapack_int flexiblas_real_LAPACKE_ztrsyl_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);
lapack_int flexiblas_chain_LAPACKE_ztrsyl_work(int matrix_layout, char trana, char tranb, lapack_int isgn, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *b, lapack_int ldb, void *c, lapack_int ldc, void *scale);

lapack_int flexiblas_real_LAPACKE_ztrtri(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ztrtri(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ztrtri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);
lapack_int flexiblas_chain_LAPACKE_ztrtri_work(int matrix_layout, char uplo, char diag, lapack_int n, void *a, lapack_int lda);

lapack_int flexiblas_real_LAPACKE_ztrtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ztrtrs(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ztrtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);
lapack_int flexiblas_chain_LAPACKE_ztrtrs_work(int matrix_layout, char uplo, char trans, char diag, lapack_int n, lapack_int nrhs, const void *a, lapack_int lda, void *b, lapack_int ldb);

lapack_int flexiblas_real_LAPACKE_ztrttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);
lapack_int flexiblas_chain_LAPACKE_ztrttf(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);

lapack_int flexiblas_real_LAPACKE_ztrttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);
lapack_int flexiblas_chain_LAPACKE_ztrttf_work(int matrix_layout, char transr, char uplo, lapack_int n, const void *a, lapack_int lda, void *arf);

lapack_int flexiblas_real_LAPACKE_ztrttp(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);
lapack_int flexiblas_chain_LAPACKE_ztrttp(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);

lapack_int flexiblas_real_LAPACKE_ztrttp_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);
lapack_int flexiblas_chain_LAPACKE_ztrttp_work(int matrix_layout, char uplo, lapack_int n, const void *a, lapack_int lda, void *ap);

lapack_int flexiblas_real_LAPACKE_ztzrzf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);
lapack_int flexiblas_chain_LAPACKE_ztzrzf(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau);

lapack_int flexiblas_real_LAPACKE_ztzrzf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_ztzrzf_work(int matrix_layout, lapack_int m, lapack_int n, void *a, lapack_int lda, void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zunbdb(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2);
lapack_int flexiblas_chain_LAPACKE_zunbdb(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2);

lapack_int flexiblas_real_LAPACKE_zunbdb_work(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zunbdb_work(int matrix_layout, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *phi, void *taup1, void *taup2, void *tauq1, void *tauq2, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zuncsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t);
lapack_int flexiblas_chain_LAPACKE_zuncsd(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t);

lapack_int flexiblas_real_LAPACKE_zuncsd2by1(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t);
lapack_int flexiblas_chain_LAPACKE_zuncsd2by1(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t);

lapack_int flexiblas_real_LAPACKE_zuncsd2by1_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_zuncsd2by1_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x21, lapack_int ldx21, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_zuncsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork);
lapack_int flexiblas_chain_LAPACKE_zuncsd_work(int matrix_layout, char jobu1, char jobu2, char jobv1t, char jobv2t, char trans, char signs, lapack_int m, lapack_int p, lapack_int q, void *x11, lapack_int ldx11, void *x12, lapack_int ldx12, void *x21, lapack_int ldx21, void *x22, lapack_int ldx22, void *theta, void *u1, lapack_int ldu1, void *u2, lapack_int ldu2, void *v1t, lapack_int ldv1t, void *v2t, lapack_int ldv2t, void *work, lapack_int lwork, void *rwork, lapack_int lrwork, void *iwork);

lapack_int flexiblas_real_LAPACKE_zungbr(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_zungbr(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_zungbr_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zungbr_work(int matrix_layout, char vect, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zunghr(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_zunghr(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_zunghr_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zunghr_work(int matrix_layout, lapack_int n, lapack_int ilo, lapack_int ihi, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zunglq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_zunglq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_zunglq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zunglq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zungql(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_zungql(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_zungql_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zungql_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zungqr(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_zungqr(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_zungqr_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zungqr_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zungrq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_zungrq(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_zungrq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zungrq_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int k, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zungtr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau);
lapack_int flexiblas_chain_LAPACKE_zungtr(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau);

lapack_int flexiblas_real_LAPACKE_zungtr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zungtr_work(int matrix_layout, char uplo, lapack_int n, void *a, lapack_int lda, const void *tau, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zungtsqr_row(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt);
lapack_int flexiblas_chain_LAPACKE_zungtsqr_row(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt);

lapack_int flexiblas_real_LAPACKE_zungtsqr_row_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zungtsqr_row_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int mb, lapack_int nb, void *a, lapack_int lda, const void *t, lapack_int ldt, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zunhr_col(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);
lapack_int flexiblas_chain_LAPACKE_zunhr_col(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);

lapack_int flexiblas_real_LAPACKE_zunhr_col_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);
lapack_int flexiblas_chain_LAPACKE_zunhr_col_work(int matrix_layout, lapack_int m, lapack_int n, lapack_int nb, void *a, lapack_int lda, void *t, lapack_int ldt, void *d);

lapack_int flexiblas_real_LAPACKE_zunmbr(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zunmbr(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zunmbr_work(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zunmbr_work(int matrix_layout, char vect, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zunmhr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zunmhr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zunmhr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zunmhr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int ilo, lapack_int ihi, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zunmlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zunmlq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zunmlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zunmlq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zunmql(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zunmql(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zunmql_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zunmql_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zunmqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zunmqr(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zunmqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zunmqr_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zunmrq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zunmrq(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zunmrq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zunmrq_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zunmrz(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zunmrz(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zunmrz_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zunmrz_work(int matrix_layout, char side, char trans, lapack_int m, lapack_int n, lapack_int k, lapack_int l, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zunmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zunmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zunmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);
lapack_int flexiblas_chain_LAPACKE_zunmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *a, lapack_int lda, const void *tau, void *c, lapack_int ldc, void *work, lapack_int lwork);

lapack_int flexiblas_real_LAPACKE_zupgtr(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq);
lapack_int flexiblas_chain_LAPACKE_zupgtr(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq);

lapack_int flexiblas_real_LAPACKE_zupgtr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq, void *work);
lapack_int flexiblas_chain_LAPACKE_zupgtr_work(int matrix_layout, char uplo, lapack_int n, const void *ap, const void *tau, void *q, lapack_int ldq, void *work);

lapack_int flexiblas_real_LAPACKE_zupmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc);
lapack_int flexiblas_chain_LAPACKE_zupmtr(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc);

lapack_int flexiblas_real_LAPACKE_zupmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc, void *work);
lapack_int flexiblas_chain_LAPACKE_zupmtr_work(int matrix_layout, char side, char uplo, char trans, lapack_int m, lapack_int n, const void *ap, const void *tau, void *c, lapack_int ldc, void *work);



#ifdef __cplusplus
}
#endif
#endif
