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

static TLS_STORE uint8_t hook_cblas_zsyrk_pos = 0;

void cblas_zsyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void *beta, void  *C, const CBLAS_INT ldc)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
         const void *alpha, const void  *A, const CBLAS_INT lda,
         const void *beta, void  *C, const CBLAS_INT ldc);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(zsyrk);

    fn(layout,Uplo,Trans,N,K,alpha,A,lda,beta,C,ldc);

}

void flexiblas_chain_cblas_zsyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void *beta, void  *C, const CBLAS_INT ldc)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
         const void *alpha, const void  *A, const CBLAS_INT lda,
         const void *beta, void  *C, const CBLAS_INT ldc);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_ADVANCE(zsyrk);

    fn(layout,Uplo,Trans,N,K,alpha,A,lda,beta,C,ldc);


}

void flexiblas_real_cblas_zsyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void *beta, void  *C, const CBLAS_INT ldc)
{
    char UL, TR;
#define F77_TR &TR
#define F77_UL &UL

#ifdef F77_INT
    F77_INT F77_N=N, F77_K=K, F77_lda=lda;
    F77_INT F77_ldc=ldc;
#else
#define F77_N N
#define F77_K K
#define F77_lda lda
#define F77_ldc ldc
#endif

    if ( current_backend->blas.zsyrk.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
             const void *alpha, const void  *A, const CBLAS_INT lda,
             const void *beta, void  *C, const CBLAS_INT ldc);
        *(void **) & fn = current_backend->blas.zsyrk.cblas_function;
        fn(layout,Uplo,Trans,N,K,alpha,A,lda,beta,C,ldc);
    } else {
        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;
        CBLAS_CallFromC = 1;

        if( layout == CblasColMajor )
        {

            if( Uplo == CblasUpper) UL='U';
            else if ( Uplo == CblasLower ) UL='L';
            else
            {
                cblas_xerbla(2, "cblas_zsyrk", "Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if( Trans == CblasTrans) TR ='T';
            else if ( Trans == CblasConjTrans ) TR='C';
            else if ( Trans == CblasNoTrans )   TR='N';
            else
            {
                cblas_xerbla(3, "cblas_zsyrk", "Illegal Trans setting, %d\n", Trans);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }


#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TR = C2F_CHAR(&TR);
#endif

            FC_GLOBAL(zsyrk,ZSYRK)(F77_UL, F77_TR, (blasint *)(uintptr_t)&F77_N, (blasint *)(uintptr_t)&F77_K, (void *)(uintptr_t) alpha, (void *)(uintptr_t) A, (blasint *)(uintptr_t)&F77_lda,
                    (void *)(uintptr_t) beta, C, (blasint *)(uintptr_t)&F77_ldc, 1, 1);
        } else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if( Uplo == CblasUpper) UL='L';
            else if ( Uplo == CblasLower ) UL='U';
            else
            {
                cblas_xerbla(3, "cblas_zsyrk", "Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
            if( Trans == CblasTrans) TR ='N';
            else if ( Trans == CblasConjTrans ) TR='N';
            else if ( Trans == CblasNoTrans )   TR='T';
            else
            {
                cblas_xerbla(3, "cblas_zsyrk", "Illegal Trans setting, %d\n", Trans);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TR = C2F_CHAR(&TR);
#endif

            FC_GLOBAL(zsyrk,ZSYRK)(F77_UL, F77_TR, (blasint *)(uintptr_t)&F77_N, (blasint *)(uintptr_t)&F77_K, (void *)(uintptr_t) alpha, (void *)(uintptr_t) A, (blasint *)(uintptr_t)&F77_lda,
                    (void *)(uintptr_t) beta, C, (blasint *)(uintptr_t)&F77_ldc, 1, 1);
        }
        else cblas_xerbla(1, "cblas_zsyrk", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;

    }
    return;
}

