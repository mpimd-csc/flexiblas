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

static TLS_STORE uint8_t hook_cblas_zgemv_pos = 0;

void cblas_zgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void  *X, const CBLAS_INT incX, const void *beta,
        void  *Y, const CBLAS_INT incY)
{
    void (*fn)
        (const CBLAS_LAYOUT layout,
         const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
         const void *alpha, const void  *A, const CBLAS_INT lda,
         const void  *X, const CBLAS_INT incX, const void *beta,
         void  *Y, const CBLAS_INT incY);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(zgemv);
    fn(layout,TransA,M,N,alpha,A,lda,X,incX,beta,Y,incY);


}

void flexiblas_chain_cblas_zgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void  *X, const CBLAS_INT incX, const void *beta,
        void  *Y, const CBLAS_INT incY)
{
    void (*fn)
        (const CBLAS_LAYOUT layout,
         const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
         const void *alpha, const void  *A, const CBLAS_INT lda,
         const void  *X, const CBLAS_INT incX, const void *beta,
         void  *Y, const CBLAS_INT incY);
    CBLAS_HOOK_ADVANCE(zgemv);
    fn(layout,TransA,M,N,alpha,A,lda,X,incX,beta,Y,incY);


}

void flexiblas_real_cblas_zgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void  *X, const CBLAS_INT incX, const void *beta,
        void  *Y, const CBLAS_INT incY)
{
    char TA;
#define F77_TA &TA
#ifdef F77_INT
    F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
#define F77_M M
#define F77_N N
#define F77_lda lda
#define F77_incX incx
#define F77_incY incY
    CBLAS_INT incx=incX;

#endif

    if ( current_backend->blas.zgemv.cblas_function != NULL ) {

        void (*fn)
            (const CBLAS_LAYOUT layout,
             const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
             const void *alpha, const void  *A, const CBLAS_INT lda,
             const void  *X, const CBLAS_INT incX, const void *beta,
             void  *Y, const CBLAS_INT incY);
        *(void **) & fn = current_backend->blas.zgemv.cblas_function;
        fn(layout,TransA,M,N,alpha,A,lda,X,incX,beta,Y,incY);
    } else {
        CBLAS_INT n=0, i=0;
        double *xx;;
        double ALPHA[2],BETA[2];
        CBLAS_INT tincY, tincx;
        double *x = NULL, *y=(double *)(uintptr_t)Y, *st=0, *tx=0;
        const double *stx = x;
        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;

        COPY_CONST_PTR(xx,X);
        COPY_CONST_PTR(x,X);

        CBLAS_CallFromC = 1;

        if (layout == CblasColMajor)
        {
            if (TransA == CblasNoTrans) TA = 'N';
            else if (TransA == CblasTrans) TA = 'T';
            else if (TransA == CblasConjTrans) TA = 'C';
            else
            {
                cblas_xerbla(2, "cblas_zgemv","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_TA = C2F_CHAR(&TA);
#endif
            FC_GLOBAL(zgemv,ZGEMV)(F77_TA, (blasint *)(uintptr_t)&F77_M, (blasint *)(uintptr_t)&F77_N, (void *)(uintptr_t) alpha, (void *)(uintptr_t) A, (blasint *)(uintptr_t)&F77_lda, (void *)(uintptr_t) X, (blasint *)(uintptr_t)&F77_incX,
                    (void *)(uintptr_t) beta, Y, (blasint *)(uintptr_t)&F77_incY, 1);
        }
        else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if (TransA == CblasNoTrans) TA = 'T';
            else if (TransA == CblasTrans) TA = 'N';
            else if (TransA == CblasConjTrans)
            {
                ALPHA[0]=    *( (const double *)  alpha    );
                ALPHA[1]= -( *( (const double *)  alpha+1) );
                BETA[0]=     *( (const double *)  beta     );
                BETA[1]= -(  *( (const double *)  beta+1 ) );
                TA = 'N';
                if (M > 0)
                {
                    n = M << 1;
                    x = malloc(n*sizeof(double));
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

                    F77_incX = 1;

                    if(incY > 0)
                        tincY = incY;
                    else
                        tincY = -incY;

                    y++;

                    if (N > 0)
                    {
                        i = tincY << 1;
                        n = i * N ;
                        st = y + n;
                        do {
                            *y = -(*y);
                            y += i;
                        } while(y != st);
                        y -= n;
                    }
                    stx = x;
                }
                else stx = (const double *)X;
            }
            else
            {
                cblas_xerbla(2, "cblas_zgemv","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_TA = C2F_CHAR(&TA);
#endif
            if (TransA == CblasConjTrans)
                FC_GLOBAL(zgemv,ZGEMV)(F77_TA, (blasint *)(uintptr_t)&F77_N, (blasint *)(uintptr_t)&F77_M, (void *)(uintptr_t) ALPHA, (void *)(uintptr_t) A, (blasint *)(uintptr_t)&F77_lda, (void *)(uintptr_t) stx,
                        (blasint *)(uintptr_t)&F77_incX, (void *)(uintptr_t) BETA, Y, (blasint *)(uintptr_t)&F77_incY, 1 );
            else
                FC_GLOBAL(zgemv,ZGEMV)(F77_TA, (blasint *)(uintptr_t)&F77_N, (blasint *)(uintptr_t)&F77_M, (void *)(uintptr_t) alpha, (void *)(uintptr_t) A, (blasint *)(uintptr_t)&F77_lda, (void *)(uintptr_t) x,
                        (blasint *)(uintptr_t)&F77_incX, (void *)(uintptr_t) beta, Y, (blasint *)(uintptr_t)&F77_incY, 1);

            if (TransA == CblasConjTrans)
            {
                if (x != (const double *)X) free(x);
                if (N > 0)
                {
                    do
                    {
                        *y = -(*y);
                        y += i;
                    }
                    while (y != st);
                }
            }
        }
        else cblas_xerbla(1, "cblas_zgemv", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;

    }
    return;
}
