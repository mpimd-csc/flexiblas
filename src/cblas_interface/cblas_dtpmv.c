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

static TLS_STORE uint8_t hook_cblas_dtpmv_pos = 0;

void cblas_dtpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const CBLAS_INT N, const double  *Ap, double  *X, const CBLAS_INT incX)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
         const CBLAS_INT N, const double  *Ap, double  *X, const CBLAS_INT incX);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(dtpmv);
    fn(layout,Uplo,TransA,Diag,N,Ap,X,incX);

}

void flexiblas_chain_cblas_dtpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const CBLAS_INT N, const double  *Ap, double  *X, const CBLAS_INT incX)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
         const CBLAS_INT N, const double  *Ap, double  *X, const CBLAS_INT incX);
    CBLAS_HOOK_ADVANCE(dtpmv);
    fn(layout,Uplo,TransA,Diag,N,Ap,X,incX);

}

void flexiblas_real_cblas_dtpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const CBLAS_INT N, const double  *Ap, double  *X, const CBLAS_INT incX)
{
    char TA;
    char UL;
    char DI;
#define F77_TA &TA
#define F77_UL &UL
#define F77_DI &DI
#ifdef F77_INT
    F77_INT F77_N=N, F77_incX=incX;
#else
#define F77_N N
#define F77_incX incX
#endif

    if ( current_backend->blas.dtpmv.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
             const CBLAS_INT N, const double  *Ap, double  *X, const CBLAS_INT incX);
        *(void **) &fn = current_backend->blas.dtpmv.cblas_function;
        fn(layout,Uplo,TransA,Diag,N,Ap,X,incX);
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
                cblas_xerbla(2, "cblas_dtpmv","Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
            if (TransA == CblasNoTrans) TA = 'N';
            else if (TransA == CblasTrans) TA = 'T';
            else if (TransA == CblasConjTrans) TA = 'C';
            else
            {
                cblas_xerbla(3, "cblas_dtpmv","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
            if (Diag == CblasUnit) DI = 'U';
            else if (Diag == CblasNonUnit) DI = 'N';
            else
            {
                cblas_xerbla(4, "cblas_dtpmv","Illegal Diag setting, %d\n", Diag);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TA = C2F_CHAR(&TA);
            F77_DI = C2F_CHAR(&DI);
#endif
            FC_GLOBAL(dtpmv,DTPMV)( F77_UL, F77_TA, F77_DI, (blasint *)(uintptr_t)&F77_N, (double *)(uintptr_t)Ap, X, (blasint *)(uintptr_t)&F77_incX, 1, 1, 1);
        }
        else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if (Uplo == CblasUpper) UL = 'L';
            else if (Uplo == CblasLower) UL = 'U';
            else
            {
                cblas_xerbla(2, "cblas_dtpmv","Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if (TransA == CblasNoTrans) TA = 'T';
            else if (TransA == CblasTrans) TA = 'N';
            else if (TransA == CblasConjTrans) TA = 'N';
            else
            {
                cblas_xerbla(3, "cblas_dtpmv","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if (Diag == CblasUnit) DI = 'U';
            else if (Diag == CblasNonUnit) DI = 'N';
            else
            {
                cblas_xerbla(4, "cblas_dtpmv","Illegal Diag setting, %d\n", Diag);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TA = C2F_CHAR(&TA);
            F77_DI = C2F_CHAR(&DI);
#endif

            FC_GLOBAL(dtpmv,DTPMV)( F77_UL, F77_TA, F77_DI, (blasint *)(uintptr_t)&F77_N, (double *)(uintptr_t)Ap, X,(blasint *)(uintptr_t)&F77_incX, 1,1,1 );
        }
        else cblas_xerbla(1, "cblas_dtpmv", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;
    }
    return;
}
