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
static TLS_STORE uint8_t hook_cblas_cher2k_pos = 0;

void cblas_cher2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
        const void *alpha, const void *A, const CBLAS_INT lda,
        const void *B, const CBLAS_INT ldb, const float beta,
        void *C, const CBLAS_INT ldc)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
         const void *alpha, const void *A, const CBLAS_INT lda,
         const void *B, const CBLAS_INT ldb, const float beta,
         void *C, const CBLAS_INT ldc);

    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(cher2k);
    fn(layout,Uplo,Trans,N,K,alpha,A,lda,B,ldb,beta,C,ldc);

}

void flexiblas_chain_cblas_cher2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
        const void *alpha, const void *A, const CBLAS_INT lda,
        const void *B, const CBLAS_INT ldb, const float beta,
        void *C, const CBLAS_INT ldc)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
         const void *alpha, const void *A, const CBLAS_INT lda,
         const void *B, const CBLAS_INT ldb, const float beta,
         void *C, const CBLAS_INT ldc);

    CBLAS_HOOK_ADVANCE(cher2k);

    fn(layout,Uplo,Trans,N,K,alpha,A,lda,B,ldb,beta,C,ldc);


}

void flexiblas_real_cblas_cher2k(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
        const void *alpha, const void *A, const CBLAS_INT lda,
        const void *B, const CBLAS_INT ldb, const float beta,
        void *C, const CBLAS_INT ldc)
{
    char UL, TR;
#define F77_TR &TR
#define F77_UL &UL

#ifdef F77_INT
    F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
    F77_INT F77_ldc=ldc;
#else
#define F77_N N
#define F77_K K
#define F77_lda lda
#define F77_ldb ldb
#define F77_ldc ldc
#endif

    if ( current_backend->blas.cher2k.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
             const void *alpha, const void *A, const CBLAS_INT lda,
             const void *B, const CBLAS_INT ldb, const float beta,
             void *C, const CBLAS_INT ldc);
        *(void **) & fn = current_backend->blas.cher2k.cblas_function;
        fn(layout,Uplo,Trans,N,K,alpha,A,lda,B,ldb,beta,C,ldc);
    } else {
        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        float ALPHA[2];
        const float *alp=(const float *)alpha;

        CBLAS_CallFromC = 1;
        RowMajorStrg = 0;

        if( layout == CblasColMajor )
        {

            if( Uplo == CblasUpper) UL='U';
            else if ( Uplo == CblasLower ) UL='L';
            else
            {
                cblas_xerbla(2, "cblas_cher2k", "Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if( Trans == CblasTrans) TR ='T';
            else if ( Trans == CblasConjTrans ) TR='C';
            else if ( Trans == CblasNoTrans )   TR='N';
            else
            {
                cblas_xerbla(3, "cblas_cher2k", "Illegal Trans setting, %d\n", Trans);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TR = C2F_CHAR(&TR);
#endif

            FC_GLOBAL(cher2k,CHER2K)(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
        } else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;

            if( Uplo == CblasUpper) UL='L';
            else if ( Uplo == CblasLower ) UL='U';
            else
            {
                cblas_xerbla(2, "cblas_cher2k", "Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
            if( Trans == CblasTrans) TR ='N';
            else if ( Trans == CblasConjTrans ) TR='N';
            else if ( Trans == CblasNoTrans )   TR='C';
            else
            {
                cblas_xerbla(3, "cblas_cher2k", "Illegal Trans setting, %d\n", Trans);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TR = C2F_CHAR(&TR);
#endif

            ALPHA[0]= *alp;
            ALPHA[1]= -alp[1];
            FC_GLOBAL(cher2k,CHER2K)(F77_UL,F77_TR, &F77_N, &F77_K, ALPHA, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
        }
        else  cblas_xerbla(1, "cblas_cher2k", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;

    }
    return;
}

