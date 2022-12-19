/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Linking FlexiBLAS statically or dynamically with other modules is making a
 * combined work based on FlexiBLAS. Thus, the terms and conditions of the GNU
 * General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2022
 */





#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_ctrsv_pos = 0;

void cblas_ctrsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const CBLAS_INT N, const void  *A, const CBLAS_INT lda, void  *X,
        const CBLAS_INT incX)
{
       void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
             const CBLAS_INT N, const void  *A, const CBLAS_INT lda, void  *X,
             const CBLAS_INT incX);

       CBLAS_BACKEND_INIT();
       CBLAS_HOOK_SELECT(ctrsv);

       fn(layout,Uplo,TransA,Diag,N,A,lda,X,incX);

}

void flexiblas_chain_cblas_ctrsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const CBLAS_INT N, const void  *A, const CBLAS_INT lda, void  *X,
        const CBLAS_INT incX)
{
       void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
             const CBLAS_INT N, const void  *A, const CBLAS_INT lda, void  *X,
             const CBLAS_INT incX);
       CBLAS_HOOK_ADVANCE(ctrsv);
       fn(layout,Uplo,TransA,Diag,N,A,lda,X,incX);

}

void flexiblas_real_cblas_ctrsv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
        const CBLAS_INT N, const void  *A, const CBLAS_INT lda, void  *X,
        const CBLAS_INT incX)
{
    char TA;
    char UL;
    char DI;
#define F77_TA &TA
#define F77_UL &UL
#define F77_DI &DI
#ifdef F77_INT
    F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
#define F77_N N
#define F77_lda lda
#define F77_incX incX
#endif

    if ( current_backend->blas.ctrsv.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
             const CBLAS_INT N, const void  *A, const CBLAS_INT lda, void  *X,
             const CBLAS_INT incX);
        *(void **) & fn = current_backend->blas.ctrsv.cblas_function;
        fn(layout,Uplo,TransA,Diag,N,A,lda,X,incX);
    } else {
        CBLAS_INT n, i=0, tincX;
        float *st=0,*x=(float *)X;
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
                cblas_xerbla(2, "cblas_ctrsv","Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
            if (TransA == CblasNoTrans) TA = 'N';
            else if (TransA == CblasTrans) TA = 'T';
            else if (TransA == CblasConjTrans) TA = 'C';
            else
            {
                cblas_xerbla(3, "cblas_ctrsv","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
            if (Diag == CblasUnit) DI = 'U';
            else if (Diag == CblasNonUnit) DI = 'N';
            else
            {
                cblas_xerbla(4, "cblas_ctrsv","Illegal Diag setting, %d\n", Diag);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TA = C2F_CHAR(&TA);
            F77_DI = C2F_CHAR(&DI);
#endif
            FC_GLOBAL(ctrsv,CTRSV)( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                    &F77_incX);
        }
        else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if (Uplo == CblasUpper) UL = 'L';
            else if (Uplo == CblasLower) UL = 'U';
            else
            {
                cblas_xerbla(2, "cblas_ctrsv","Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if (TransA == CblasNoTrans) TA = 'T';
            else if (TransA == CblasTrans) TA = 'N';
            else if (TransA == CblasConjTrans)
            {
                TA = 'N';
                if ( N > 0)
                {
                    if ( incX > 0 )
                        tincX = incX;
                    else
                        tincX = -incX;

                    n = N*2*(tincX);
                    x++;
                    st=x+n;
                    i = tincX << 1;
                    do
                    {
                        *x = -(*x);
                        x+=i;
                    }
                    while (x != st);
                    x -= n;
                }
            }
            else
            {
                cblas_xerbla(3, "cblas_ctrsv","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if (Diag == CblasUnit) DI = 'U';
            else if (Diag == CblasNonUnit) DI = 'N';
            else
            {
                cblas_xerbla(4, "cblas_ctrsv","Illegal Diag setting, %d\n", Diag);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TA = C2F_CHAR(&TA);
            F77_DI = C2F_CHAR(&DI);
#endif
            FC_GLOBAL(ctrsv,CTRSV)( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                    &F77_incX);
            if (TransA == CblasConjTrans)
            {
                if (N > 0)
                {
                    do
                    {
                        *x = -(*x);
                        x += i;
                    }
                    while (x != st);
                }
            }
        }
        else cblas_xerbla(1, "cblas_ctrsv", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;

    }
    return;
}
