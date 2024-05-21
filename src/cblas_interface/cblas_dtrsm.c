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

static TLS_STORE uint8_t hook_cblas_dtrsm_pos = 0;

void cblas_dtrsm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
        const double alpha, const double  *A, const CBLAS_INT lda,
        double  *B, const CBLAS_INT ldb)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
         const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
         const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
         const double alpha, const double  *A, const CBLAS_INT lda,
         double  *B, const CBLAS_INT ldb);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(dtrsm);
    fn(layout,Side,Uplo,TransA,Diag,M,N,alpha,A,lda,B,ldb);

}

void flexiblas_chain_cblas_dtrsm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
        const double alpha, const double  *A, const CBLAS_INT lda,
        double  *B, const CBLAS_INT ldb)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
         const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
         const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
         const double alpha, const double  *A, const CBLAS_INT lda,
         double  *B, const CBLAS_INT ldb);
    CBLAS_HOOK_ADVANCE(dtrsm);
    fn(layout,Side,Uplo,TransA,Diag,M,N,alpha,A,lda,B,ldb);

}


void flexiblas_real_cblas_dtrsm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
        const double alpha, const double  *A, const CBLAS_INT lda,
        double  *B, const CBLAS_INT ldb)
{
    char UL, TA, SD, DI;
#define F77_TA &TA
#define F77_UL &UL
#define F77_SD &SD
#define F77_DI &DI

#ifdef F77_INT
    F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
#else
#define F77_M M
#define F77_N N
#define F77_lda lda
#define F77_ldb ldb
#endif
    if ( current_backend->blas.dtrsm.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
             const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
             const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
             const double alpha, const double  *A, const CBLAS_INT lda,
             double  *B, const CBLAS_INT ldb);
        *(void **) &fn = current_backend->blas.dtrsm.cblas_function;
        fn(layout,Side,Uplo,TransA,Diag,M,N,alpha,A,lda,B,ldb);
    } else {

        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;
        CBLAS_CallFromC = 1;

        if( layout == CblasColMajor )
        {
            if      ( Side == CblasRight) SD='R';
            else if ( Side == CblasLeft ) SD='L';
            else
            {
                cblas_xerbla(2, "cblas_dtrsm","Illegal Side setting, %d\n", Side);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
            if      ( Uplo == CblasUpper) UL='U';
            else if ( Uplo == CblasLower) UL='L';
            else
            {
                cblas_xerbla(3, "cblas_dtrsm","Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if      ( TransA == CblasTrans    ) TA='T';
            else if ( TransA == CblasConjTrans) TA='C';
            else if ( TransA == CblasNoTrans  ) TA='N';
            else
            {
                cblas_xerbla(4, "cblas_dtrsm","Illegal Trans setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if      ( Diag == CblasUnit   ) DI='U';
            else if ( Diag == CblasNonUnit) DI='N';
            else
            {
                cblas_xerbla(5, "cblas_dtrsm","Illegal Diag setting, %d\n", Diag);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            FC_GLOBAL(dtrsm,DTRSM)(F77_SD, F77_UL, F77_TA, F77_DI, (blasint *)(uintptr_t)&F77_M, (blasint *)(uintptr_t)&F77_N, (double *)(uintptr_t)&alpha, (double *)(uintptr_t)A, (blasint *)(uintptr_t)&F77_lda, B, (blasint *)(uintptr_t)&F77_ldb, 1, 1, 1, 1);
        }
        else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if      ( Side == CblasRight) SD='L';
            else if ( Side == CblasLeft ) SD='R';
            else
            {
                cblas_xerbla(2, "cblas_dtrsm","Illegal Side setting, %d\n", Side);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if      ( Uplo == CblasUpper) UL='L';
            else if ( Uplo == CblasLower) UL='U';
            else
            {
                cblas_xerbla(3, "cblas_dtrsm","Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if      ( TransA == CblasTrans    ) TA='T';
            else if ( TransA == CblasConjTrans) TA='C';
            else if ( TransA == CblasNoTrans  ) TA='N';
            else
            {
                cblas_xerbla(4, "cblas_dtrsm","Illegal Trans setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if      ( Diag == CblasUnit   ) DI='U';
            else if ( Diag == CblasNonUnit) DI='N';
            else
            {
                cblas_xerbla(5, "cblas_dtrsm","Illegal Diag setting, %d\n", Diag);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            FC_GLOBAL(dtrsm,DTRSM)(F77_SD, F77_UL, F77_TA, F77_DI, (blasint *)(uintptr_t)&F77_N, (blasint *)(uintptr_t)&F77_M, (double *)(uintptr_t)&alpha, (double *)(uintptr_t)A, (blasint *)(uintptr_t)&F77_lda, B, (blasint *)(uintptr_t)&F77_ldb, 1, 1, 1, 1);
        }
        else cblas_xerbla(1, "cblas_dtrsm","Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;
    }
    return;
}
