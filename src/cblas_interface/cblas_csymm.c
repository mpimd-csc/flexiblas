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

static TLS_STORE uint8_t hook_cblas_csymm_pos = 0;

void cblas_csymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void  *B, const CBLAS_INT ldb, const void *beta,
        void  *C, const CBLAS_INT ldc)
{
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
             const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
             const void *alpha, const void  *A, const CBLAS_INT lda,
             const void  *B, const CBLAS_INT ldb, const void *beta,
             void  *C, const CBLAS_INT ldc);

        CBLAS_BACKEND_INIT();
        CBLAS_HOOK_SELECT(csymm);

        fn(layout,Side,Uplo,M,N,alpha,A,lda,B,ldb,beta,C,ldc);

}

void flexiblas_chain_cblas_csymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void  *B, const CBLAS_INT ldb, const void *beta,
        void  *C, const CBLAS_INT ldc)
{
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
             const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
             const void *alpha, const void  *A, const CBLAS_INT lda,
             const void  *B, const CBLAS_INT ldb, const void *beta,
             void  *C, const CBLAS_INT ldc);

        CBLAS_HOOK_ADVANCE(csymm);

        fn(layout,Side,Uplo,M,N,alpha,A,lda,B,ldb,beta,C,ldc);

}


void flexiblas_real_cblas_csymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void  *B, const CBLAS_INT ldb, const void *beta,
        void  *C, const CBLAS_INT ldc)
{
    char SD, UL;
#define F77_SD &SD
#define F77_UL &UL

#ifdef F77_INT
    F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
    F77_INT F77_ldc=ldc;
#else
#define F77_M M
#define F77_N N
#define F77_lda lda
#define F77_ldb ldb
#define F77_ldc ldc
#endif
    if ( current_backend->blas.csymm.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
             const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
             const void *alpha, const void  *A, const CBLAS_INT lda,
             const void  *B, const CBLAS_INT ldb, const void *beta,
             void  *C, const CBLAS_INT ldc);
        *(void **) & fn = current_backend->blas.csymm.cblas_function;
        fn(layout,Side,Uplo,M,N,alpha,A,lda,B,ldb,beta,C,ldc);
    } else {

        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;
        CBLAS_CallFromC = 1;

        if( layout == CblasColMajor )
        {
            if( Side == CblasRight) SD='R';
            else if ( Side == CblasLeft ) SD='L';
            else
            {
                cblas_xerbla(2, "cblas_csymm", "Illegal Side setting, %d\n", Side);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if( Uplo == CblasUpper) UL='U';
            else if ( Uplo == CblasLower ) UL='L';
            else
            {
                cblas_xerbla(3, "cblas_csymm", "Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_SD = C2F_CHAR(&SD);
#endif

            FC_GLOBAL(csymm,CSYMM)(F77_SD, F77_UL, &F77_M, &F77_N, alpha, A, &F77_lda,
                    B, &F77_ldb, beta, C, &F77_ldc, 1, 1);
        } else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if( Side == CblasRight) SD='L';
            else if ( Side == CblasLeft ) SD='R';
            else
            {
                cblas_xerbla(2, "cblas_csymm", "Illegal Side setting, %d\n", Side);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if( Uplo == CblasUpper) UL='L';
            else if ( Uplo == CblasLower ) UL='U';
            else
            {
                cblas_xerbla(3, "cblas_csymm", "Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_SD = C2F_CHAR(&SD);
#endif

            FC_GLOBAL(csymm,CSYMM)(F77_SD, F77_UL, &F77_N, &F77_M, alpha, A, &F77_lda,
                    B, &F77_ldb, beta, C, &F77_ldc, 1, 1);
        }
        else cblas_xerbla(1, "cblas_csymm", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;


    }
    return;
}
