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







#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_sgemmt_pos = 0;


void cblas_sgemmt(const CBLAS_LAYOUT layout, const CBLAS_UPLO uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const CBLAS_INT N,
        const CBLAS_INT K, const float alpha, const float  *A,
        const CBLAS_INT lda, const float  *B, const CBLAS_INT ldb,
        const float beta, float  *C, const CBLAS_INT ldc)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO uplo, const CBLAS_TRANSPOSE TransA,
         const CBLAS_TRANSPOSE TransB, const CBLAS_INT N,
         const CBLAS_INT K, const float alpha, const float  *A,
         const CBLAS_INT lda, const float  *B, const CBLAS_INT ldb,
         const float beta, float  *C, const CBLAS_INT ldc);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(sgemmt);

    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);

}

void flexiblas_chain_cblas_sgemmt(const CBLAS_LAYOUT layout, const CBLAS_UPLO uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const CBLAS_INT N,
        const CBLAS_INT K, const float alpha, const float  *A,
        const CBLAS_INT lda, const float  *B, const CBLAS_INT ldb,
        const float beta, float  *C, const CBLAS_INT ldc)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO uplo, const CBLAS_TRANSPOSE TransA,
         const CBLAS_TRANSPOSE TransB, const CBLAS_INT N,
         const CBLAS_INT K, const float alpha, const float  *A,
         const CBLAS_INT lda, const float  *B, const CBLAS_INT ldb,
         const float beta, float  *C, const CBLAS_INT ldc);
    CBLAS_HOOK_ADVANCE(sgemmt);
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);

}

void flexiblas_real_cblas_sgemmt(const CBLAS_LAYOUT layout, const CBLAS_UPLO uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const CBLAS_INT N,
        const CBLAS_INT K, const float alpha, const float  *A,
        const CBLAS_INT lda, const float  *B, const CBLAS_INT ldb,
        const float beta, float  *C, const CBLAS_INT ldc)
{
    char TA, TB, UL;
#define F77_TA &TA
#define F77_TB &TB
#define F77_UL &UL


#ifdef F77_INT
    F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
    F77_INT F77_ldc=ldc;
#else
#define F77_M M
#define F77_N N
#define F77_K K
#define F77_lda lda
#define F77_ldb ldb
#define F77_ldc ldc
#endif
    if ( current_backend->blas.sgemmt.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO uplo, const CBLAS_TRANSPOSE TransA,
             const CBLAS_TRANSPOSE TransB, const CBLAS_INT N,
             const CBLAS_INT K, const float alpha, const float  *A,
             const CBLAS_INT lda, const float  *B, const CBLAS_INT ldb,
             const float beta, float  *C, const CBLAS_INT ldc);
        *(void **) &fn = current_backend->blas.sgemmt.cblas_function;
        fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {

        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;
        CBLAS_CallFromC = 1;

        if( layout == CblasColMajor )
        {
            if ( uplo == CblasUpper ) UL = 'U';
            else if (uplo == CblasLower) UL= 'L';
            else {
                cblas_xerbla(2, "cblas_sgemmt", "Illegal Uplo setting, %d\n", uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }


            if(TransA == CblasTrans) TA='T';
            else if ( TransA == CblasConjTrans ) TA='C';
            else if ( TransA == CblasNoTrans )   TA='N';
            else
            {
                cblas_xerbla(3, "cblas_sgemmt","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if(TransB == CblasTrans) TB='T';
            else if ( TransB == CblasConjTrans ) TB='C';
            else if ( TransB == CblasNoTrans )   TB='N';
            else
            {
                cblas_xerbla(4, "cblas_sgemmt","Illegal TransB setting, %d\n", TransB);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            FC_GLOBAL(sgemmt,SGEMMT)(F77_UL, F77_TA, F77_TB, (blasint *)(uintptr_t)&F77_N, (blasint *)(uintptr_t)&F77_K, (float *)(uintptr_t) &alpha,
                    (float *)(uintptr_t) A,  (blasint *)(uintptr_t)&F77_lda, (float *)(uintptr_t) B, (blasint *)(uintptr_t)&F77_ldb,
                    (float *)(uintptr_t) &beta, C, (blasint *)(uintptr_t)&F77_ldc, 1, 1, 1);
        } else if (layout == CblasRowMajor)
        {
            if ( uplo == CblasUpper ) UL = 'L';
            else if (uplo == CblasLower) UL= 'U';
            else {
                cblas_xerbla(2, "cblas_sgemmt", "Illegal Uplo setting, %d\n", uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }


            RowMajorStrg = 1;
            if(TransA == CblasTrans) TB='T';
            else if ( TransA == CblasConjTrans ) TB='C';
            else if ( TransA == CblasNoTrans )   TB='N';
            else
            {
                cblas_xerbla(3, "cblas_sgemmt",
                        "Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
            if(TransB == CblasTrans) TA='T';
            else if ( TransB == CblasConjTrans ) TA='C';
            else if ( TransB == CblasNoTrans )   TA='N';
            else
            {
                cblas_xerbla(4, "cblas_sgemmt",
                        "Illegal TransB setting, %d\n", TransB);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            FC_GLOBAL(sgemmt,SGEMMT)(F77_UL, F77_TA, F77_TB,(blasint *)(uintptr_t) &F77_N,(blasint *)(uintptr_t) &F77_K,(float *) (uintptr_t) &alpha,
                    (float *)(uintptr_t) B, (blasint *)(uintptr_t)&F77_ldb,(float *)(uintptr_t) A,(blasint *)(uintptr_t) &F77_lda,(float *)(uintptr_t) &beta,
                    (float *)(uintptr_t) C, (blasint *)(uintptr_t)&F77_ldc, 1, 1, 1);

        }
        else  cblas_xerbla(1, "cblas_sgemmt", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;
    }
    return;
}

