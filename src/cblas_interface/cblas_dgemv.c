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

static TLS_STORE uint8_t hook_cblas_dgemv_pos = 0;

void cblas_dgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
        const double alpha, const double  *A, const CBLAS_INT lda,
        const double  *X, const CBLAS_INT incX, const double beta,
        double  *Y, const CBLAS_INT incY)
{
    void (*fn)(const CBLAS_LAYOUT layout,
            const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
            const double alpha, const double  *A, const CBLAS_INT lda,
            const double  *X, const CBLAS_INT incX, const double beta,
            double  *Y, const CBLAS_INT incY);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(dgemv);

    fn ( layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y,incY);

}

void flexiblas_chain_cblas_dgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
        const double alpha, const double  *A, const CBLAS_INT lda,
        const double  *X, const CBLAS_INT incX, const double beta,
        double  *Y, const CBLAS_INT incY)
{
    void (*fn)(const CBLAS_LAYOUT layout,
            const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
            const double alpha, const double  *A, const CBLAS_INT lda,
            const double  *X, const CBLAS_INT incX, const double beta,
            double  *Y, const CBLAS_INT incY);
    CBLAS_HOOK_ADVANCE(dgemv);
    fn ( layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y,incY);

}

void flexiblas_real_cblas_dgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
        const double alpha, const double  *A, const CBLAS_INT lda,
        const double  *X, const CBLAS_INT incX, const double beta,
        double  *Y, const CBLAS_INT incY)
{
    char TA;
#define F77_TA &TA
#ifdef F77_INT
    F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
#define F77_M M
#define F77_N N
#define F77_lda lda
#define F77_incX incX
#define F77_incY incY
#endif
    if ( current_backend->blas.dgemv.cblas_function != NULL ) {
        void (*fn)(const CBLAS_LAYOUT layout,
                const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
                const double alpha, const double  *A, const CBLAS_INT lda,
                const double  *X, const CBLAS_INT incX, const double beta,
                double  *Y, const CBLAS_INT incY);
        *(void **) &fn = current_backend->blas.dgemv.cblas_function;
        fn ( layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y,incY);
    } else {
        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;

        CBLAS_CallFromC = 1;
        if (layout == CblasColMajor)
        {
            if (TransA == CblasNoTrans) TA = 'N';
            else if (TransA == CblasTrans) TA = 'T';
            else if (TransA == CblasConjTrans) TA = 'C';
            else
            {
                cblas_xerbla(2, "cblas_dgemv","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_TA = C2F_CHAR(&TA);
#endif
            FC_GLOBAL(dgemv,DGEMV)(F77_TA, (blasint *)(uintptr_t)&F77_M, (blasint *)(uintptr_t)&F77_N, (double *)(uintptr_t)&alpha, (double *)(uintptr_t)A, (blasint *)(uintptr_t)&F77_lda, (double *)(uintptr_t)X, (blasint *)(uintptr_t)&F77_incX, (double *)(uintptr_t)&beta, Y, (blasint *)(uintptr_t)&F77_incY, 1);
        }
        else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if (TransA == CblasNoTrans) TA = 'T';
            else if (TransA == CblasTrans) TA = 'N';
            else if (TransA == CblasConjTrans) TA = 'N';
            else
            {
                cblas_xerbla(2, "cblas_dgemv","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_TA = C2F_CHAR(&TA);
#endif
            FC_GLOBAL(dgemv,DGEMV)(F77_TA, (blasint *)(uintptr_t)&F77_N, (blasint *)(uintptr_t)&F77_M, (double *)(uintptr_t)&alpha, (double *)(uintptr_t)A, (blasint *)(uintptr_t)&F77_lda, (double *)(uintptr_t)X, (blasint *)(uintptr_t)&F77_incX, (double *)(uintptr_t)&beta, Y, (blasint *)(uintptr_t)&F77_incY, 1);
        }
        else cblas_xerbla(1, "cblas_dgemv", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;
    }
    return;
}
