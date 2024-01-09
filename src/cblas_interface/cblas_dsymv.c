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

static TLS_STORE uint8_t hook_cblas_dsymv_pos = 0;

void cblas_dsymv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const CBLAS_INT N,
        const double alpha, const double  *A, const CBLAS_INT lda,
        const double  *X, const CBLAS_INT incX, const double beta,
        double  *Y, const CBLAS_INT incY)
{
    void (*fn)
        (const CBLAS_LAYOUT layout,
         const CBLAS_UPLO Uplo, const CBLAS_INT N,
         const double alpha, const double  *A, const CBLAS_INT lda,
         const double  *X, const CBLAS_INT incX, const double beta,
         double  *Y, const CBLAS_INT incY);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(dsymv);
    fn(layout,Uplo,N,alpha,A,lda,X,incX,beta,Y,incY);

}

void flexiblas_chain_cblas_dsymv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const CBLAS_INT N,
        const double alpha, const double  *A, const CBLAS_INT lda,
        const double  *X, const CBLAS_INT incX, const double beta,
        double  *Y, const CBLAS_INT incY)
{
    void (*fn)
        (const CBLAS_LAYOUT layout,
         const CBLAS_UPLO Uplo, const CBLAS_INT N,
         const double alpha, const double  *A, const CBLAS_INT lda,
         const double  *X, const CBLAS_INT incX, const double beta,
         double  *Y, const CBLAS_INT incY);
    CBLAS_HOOK_ADVANCE(dsymv);
    fn(layout,Uplo,N,alpha,A,lda,X,incX,beta,Y,incY);

}

void flexiblas_real_cblas_dsymv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const CBLAS_INT N,
        const double alpha, const double  *A, const CBLAS_INT lda,
        const double  *X, const CBLAS_INT incX, const double beta,
        double  *Y, const CBLAS_INT incY)
{
    char UL;
#define F77_UL &UL
#ifdef F77_INT
    F77_INT F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
#define F77_N N
#define F77_lda lda
#define F77_incX incX
#define F77_incY incY
#endif

    if ( current_backend->blas.dsymv.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout,
             const CBLAS_UPLO Uplo, const CBLAS_INT N,
             const double alpha, const double  *A, const CBLAS_INT lda,
             const double  *X, const CBLAS_INT incX, const double beta,
             double  *Y, const CBLAS_INT incY);
        *(void **) &fn = current_backend->blas.dsymv.cblas_function;
        fn(layout,Uplo,N,alpha,A,lda,X,incX,beta,Y,incY);
    } else {

        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;

        CBLAS_CallFromC = 1;
        if (layout == CblasColMajor)
        {
            if (Uplo == CblasUpper) UL = 'U';
            else if (Uplo == CblasLower) UL = 'L';
            else
            {
                cblas_xerbla(2, "cblas_dsymv","Illegal Uplo setting, %d\n",Uplo );
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
#endif
            FC_GLOBAL(dsymv,DSYMV)(F77_UL, &F77_N, &alpha, A, &F77_lda, X,
                    &F77_incX, &beta, Y, &F77_incY);
        }
        else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if (Uplo == CblasUpper) UL = 'L';
            else if (Uplo == CblasLower) UL = 'U';
            else
            {
                cblas_xerbla(2, "cblas_dsymv","Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
#endif
            FC_GLOBAL(dsymv,DSYMV)(F77_UL, &F77_N, &alpha,
                    A ,&F77_lda, X,&F77_incX, &beta, Y, &F77_incY);
        }
        else cblas_xerbla(1, "cblas_dsymv", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;
    }
    return;
}
