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

static TLS_STORE uint8_t hook_cblas_cher_pos = 0;

void cblas_cher(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX
        ,void *A, const CBLAS_INT lda)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX
         ,void *A, const CBLAS_INT lda);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(cher);
    fn(layout,Uplo,N,alpha,X,incX,A,lda);

}

void flexiblas_chain_cblas_cher(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX
        ,void *A, const CBLAS_INT lda)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX
         ,void *A, const CBLAS_INT lda);
    CBLAS_HOOK_ADVANCE(cher);
    fn(layout,Uplo,N,alpha,X,incX,A,lda);
}

void flexiblas_real_cblas_cher(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX
        ,void *A, const CBLAS_INT lda)
{
    char UL;
#define F77_UL &UL

#ifdef F77_INT
    F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
#define F77_N N
#define F77_lda lda
#define F77_incX incx
    CBLAS_INT incx=incX;

#endif

    if ( current_backend->blas.cher.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX
             ,void *A, const CBLAS_INT lda);
        *(void **) &fn = current_backend->blas.cher.cblas_function;
        fn(layout,Uplo,N,alpha,X,incX,A,lda);
    } else {
        CBLAS_INT n, i, tincx;
        float *x, *xx, *tx, *st;

        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;

        COPY_CONST_PTR(x,X);
        COPY_CONST_PTR(xx,X);

        CBLAS_CallFromC = 1;
        if (layout == CblasColMajor)
        {
            if (Uplo == CblasLower) UL = 'L';
            else if (Uplo == CblasUpper) UL = 'U';
            else
            {
                cblas_xerbla(2, "cblas_cher","Illegal Uplo setting, %d\n",Uplo );
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
#endif

            FC_GLOBAL(cher,CHER)(F77_UL, (blasint *)(uintptr_t)&F77_N, (void *)(uintptr_t) &alpha, (void *)(uintptr_t) X, (blasint *)(uintptr_t)&F77_incX, A, (blasint *)(uintptr_t)&F77_lda, 1);

        }  else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if (Uplo == CblasUpper) UL = 'L';
            else if (Uplo == CblasLower) UL = 'U';
            else
            {
                cblas_xerbla(2, "cblas_cher","Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
#endif
            if (N > 0)
            {
                n = N << 1;
                x = malloc(n*sizeof(float));
                tx = x;
                if( incX > 0 ) {
                    i = incX << 1 ;
                    tincx = 2;
                    st= x+n;
                } else {
                    i = incX *(-2);
                    tincx = -2;
                    st = x-2;
                    x +=(n-2);
                }
                do
                {
                    *x = *xx;
                    x[1] = -xx[1];
                    x += tincx ;
                    xx += i;
                }
                while (x != st);
                x=tx;

#ifdef F77_INT
                F77_incX = 1;
#else
                incx = 1;
#endif
            }
            else  {
                COPY_CONST_PTR(x,X);
            }
            FC_GLOBAL(cher,CHER)(F77_UL, (blasint *)(uintptr_t)&F77_N, (void *)(uintptr_t) &alpha, (void *)(uintptr_t) x, (blasint *)(uintptr_t)&F77_incX, A, (blasint *)(uintptr_t)&F77_lda, 1);
        } else
        {
            cblas_xerbla(1, "cblas_cher","Illegal layout setting, %d\n", layout);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
        if(X!=x)
            free(x);

        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;

    }
    return;
}
