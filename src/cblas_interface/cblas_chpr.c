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

static TLS_STORE uint8_t hook_cblas_chpr_pos = 0;

void cblas_chpr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const float alpha, const void *X,
        const int incX, void *A)
{
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const int N, const float alpha, const void *X,
             const int incX, void *A);

        CBLAS_BACKEND_INIT();
        CBLAS_HOOK_SELECT(chpr);
        fn(layout,Uplo,N,alpha,X,incX,A);


}

void flexiblas_chain_cblas_chpr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const float alpha, const void *X,
        const int incX, void *A)
{
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const int N, const float alpha, const void *X,
             const int incX, void *A);
        CBLAS_HOOK_SELECT(chpr);
        fn(layout,Uplo,N,alpha,X,incX,A);

}

void flexiblas_real_cblas_chpr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const float alpha, const void *X,
        const int incX, void *A)
{
    char UL;
#define F77_UL &UL

#ifdef F77_INT
    F77_INT F77_N=N, F77_incX=incX;
#else
#define F77_N N
#define F77_incX incx
#endif
    if ( current_backend->blas.chpr.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const int N, const float alpha, const void *X,
             const int incX, void *A)
            = current_backend->blas.chpr.cblas_function;
        fn(layout,Uplo,N,alpha,X,incX,A);

    } else {
        int n, i, tincx, incx=incX;
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
                cblas_xerbla(2, "cblas_chpr","Illegal Uplo setting, %d\n",Uplo );
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
#endif

            FC_GLOBAL(chpr,CHPR)(F77_UL, &F77_N, &alpha, X, &F77_incX, A);

        }  else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if (Uplo == CblasUpper) UL = 'L';
            else if (Uplo == CblasLower) UL = 'U';
            else
            {
                cblas_xerbla(2, "cblas_chpr","Illegal Uplo setting, %d\n", Uplo);
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
                    i = incX << 1;
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
            else {
                COPY_CONST_PTR(x,X);
            }

            FC_GLOBAL(chpr,CHPR)(F77_UL, &F77_N, &alpha, x, &F77_incX, A);

        } else
        {
            cblas_xerbla(1, "cblas_chpr","Illegal layout setting, %d\n", layout);
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
