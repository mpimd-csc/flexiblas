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

static TLS_STORE uint8_t hook_cblas_dsyr_pos = 0;

void cblas_dsyr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_INT N, const double  alpha, const double  *X,
        const CBLAS_INT incX, double  *A, const CBLAS_INT lda)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const CBLAS_INT N, const double  alpha, const double  *X,
         const CBLAS_INT incX, double  *A, const CBLAS_INT lda);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(dsyr);
    fn(layout,Uplo,N,alpha,X,incX,A,lda);

}

void flexiblas_chain_cblas_dsyr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_INT N, const double  alpha, const double  *X,
        const CBLAS_INT incX, double  *A, const CBLAS_INT lda)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const CBLAS_INT N, const double  alpha, const double  *X,
         const CBLAS_INT incX, double  *A, const CBLAS_INT lda);
    CBLAS_HOOK_ADVANCE(dsyr);
    fn(layout,Uplo,N,alpha,X,incX,A,lda);

}

void flexiblas_real_cblas_dsyr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_INT N, const double  alpha, const double  *X,
        const CBLAS_INT incX, double  *A, const CBLAS_INT lda)
{
    char UL;
#define F77_UL &UL

#ifdef F77_INT
    F77_INT F77_N=N, F77_incX=incX, F77_lda=lda;
#else
#define F77_N N
#define F77_incX incX
#define F77_lda  lda
#endif
    if ( current_backend->blas.dsyr.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const CBLAS_INT N, const double  alpha, const double  *X,
             const CBLAS_INT incX, double  *A, const CBLAS_INT lda);
        *(void **) &fn = current_backend->blas.dsyr.cblas_function;
        fn(layout,Uplo,N,alpha,X,incX,A,lda);
    } else {

        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;
        CBLAS_CallFromC = 1;
        if (layout == CblasColMajor)
        {
            if (Uplo == CblasLower) UL = 'L';
            else if (Uplo == CblasUpper) UL = 'U';
            else
            {
                cblas_xerbla(2, "cblas_dsyr","Illegal Uplo setting, %d\n",Uplo );
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
#endif

            FC_GLOBAL(dsyr,DSYR)(F77_UL, (blasint *)(uintptr_t)&F77_N, (double *)(uintptr_t)&alpha, (double *)(uintptr_t)X, (blasint *)(uintptr_t)&F77_incX, A, (blasint *)(uintptr_t)&F77_lda, 1);

        }  else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if (Uplo == CblasLower) UL = 'U';
            else if (Uplo == CblasUpper) UL = 'L';
            else
            {
                cblas_xerbla(2, "cblas_dsyr","Illegal Uplo setting, %d\n",Uplo );
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
#endif
            FC_GLOBAL(dsyr,DSYR)(F77_UL, (blasint *)(uintptr_t)&F77_N, (double *)(uintptr_t)&alpha, (double *)(uintptr_t)X, (blasint *)(uintptr_t)&F77_incX, A, (blasint *)(uintptr_t)&F77_lda, 1);
        } else cblas_xerbla(1, "cblas_dsyr", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;
    }
    return;
}
