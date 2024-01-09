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





#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_dgemm_pos = 0;


void cblas_dgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N,
        const CBLAS_INT K, const double alpha, const double  *A,
        const CBLAS_INT lda, const double  *B, const CBLAS_INT ldb,
        const double beta, double  *C, const CBLAS_INT ldc)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
         const CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N,
         const CBLAS_INT K, const double alpha, const double  *A,
         const CBLAS_INT lda, const double  *B, const CBLAS_INT ldb,
         const double beta, double  *C, const CBLAS_INT ldc);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(dgemm);

    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);

}

void flexiblas_chain_cblas_dgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N,
        const CBLAS_INT K, const double alpha, const double  *A,
        const CBLAS_INT lda, const double  *B, const CBLAS_INT ldb,
        const double beta, double  *C, const CBLAS_INT ldc)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
         const CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N,
         const CBLAS_INT K, const double alpha, const double  *A,
         const CBLAS_INT lda, const double  *B, const CBLAS_INT ldb,
         const double beta, double  *C, const CBLAS_INT ldc);
    CBLAS_HOOK_ADVANCE(dgemm);
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);

}

void flexiblas_real_cblas_dgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N,
        const CBLAS_INT K, const double alpha, const double  *A,
        const CBLAS_INT lda, const double  *B, const CBLAS_INT ldb,
        const double beta, double  *C, const CBLAS_INT ldc)
{
    char TA, TB;
#define F77_TA &TA
#define F77_TB &TB

#ifdef F77_INT
    F77_INT F77_M=M, F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
    F77_INT F77_ldc=ldc;
#else
#define F77_M M
#define F77_N N
#define F77_K K
#define F77_lda lda
#define F77_ldb ldb
#define F77_ldc ldc
#endif
    if ( current_backend->blas.dgemm.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
             const CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N,
             const CBLAS_INT K, const double alpha, const double  *A,
             const CBLAS_INT lda, const double  *B, const CBLAS_INT ldb,
             const double beta, double  *C, const CBLAS_INT ldc);
        *(void **) &fn = current_backend->blas.dgemm.cblas_function;
        fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {

        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;
        CBLAS_CallFromC = 1;

        if( layout == CblasColMajor )
        {
            if(TransA == CblasTrans) TA='T';
            else if ( TransA == CblasConjTrans ) TA='C';
            else if ( TransA == CblasNoTrans )   TA='N';
            else
            {
                cblas_xerbla(2, "cblas_dgemm","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if(TransB == CblasTrans) TB='T';
            else if ( TransB == CblasConjTrans ) TB='C';
            else if ( TransB == CblasNoTrans )   TB='N';
            else
            {
                cblas_xerbla(3, "cblas_dgemm","Illegal TransB setting, %d\n", TransB);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            FC_GLOBAL(dgemm,DGEMM)(F77_TA, F77_TB, &F77_M, &F77_N, &F77_K, &alpha, A,  &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
        } else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if(TransA == CblasTrans) TB='T';
            else if ( TransA == CblasConjTrans ) TB='C';
            else if ( TransA == CblasNoTrans )   TB='N';
            else
            {
                cblas_xerbla(2, "cblas_dgemm","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
            if(TransB == CblasTrans) TA='T';
            else if ( TransB == CblasConjTrans ) TA='C';
            else if ( TransB == CblasNoTrans )   TA='N';
            else
            {
                cblas_xerbla(2, "cblas_dgemm","Illegal TransB setting, %d\n", TransB);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            FC_GLOBAL(dgemm,DGEMM)(F77_TA, F77_TB, &F77_N, &F77_M, &F77_K, &alpha, B,  &F77_ldb, A, &F77_lda, &beta, C, &F77_ldc);
        }
        else  cblas_xerbla(1, "cblas_dgemm", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;
    }
    return;
}

