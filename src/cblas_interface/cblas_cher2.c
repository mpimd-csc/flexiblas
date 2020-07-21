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
 * Copyright (C) Martin Koehler, 2013-2020
 */



#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_cher2_pos = 0;

void cblas_cher2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const int N, const void *alpha, const void *X, const int incX,
         const void *Y, const int incY, void *A, const int lda);

    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(cher2);
    fn(layout,Uplo,N,alpha,X,incX,Y,incY,A,lda);


}

void flexiblas_chain_cblas_cher2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
         const int N, const void *alpha, const void *X, const int incX,
         const void *Y, const int incY, void *A, const int lda);
    CBLAS_HOOK_ADVANCE(cher2);
    fn(layout,Uplo,N,alpha,X,incX,Y,incY,A,lda);

}

void flexiblas_real_cblas_cher2(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const int N, const void *alpha, const void *X, const int incX,
        const void *Y, const int incY, void *A, const int lda)
{
    char UL;
#define F77_UL &UL

#ifdef F77_INT
    F77_INT F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
#define F77_N N
#define F77_lda lda
#define F77_incX incx
#define F77_incY incy
#endif
    if ( current_backend->blas.cher2.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const int N, const void *alpha, const void *X, const int incX,
             const void *Y, const int incY, void *A, const int lda)
            = current_backend->blas.cher2.cblas_function;
        fn(layout,Uplo,N,alpha,X,incX,Y,incY,A,lda);
    } else {
        int n, i, j, tincx, tincy, incx=incX, incy=incY;
        float *x, *xx, *y, *yy, *tx, *ty, *stx, *sty;

        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;

        COPY_CONST_PTR(x,X);
        COPY_CONST_PTR(xx,X);
        COPY_CONST_PTR(y,Y);
        COPY_CONST_PTR(yy,Y);

        CBLAS_CallFromC = 1;
        if (layout == CblasColMajor)
        {
            if (Uplo == CblasLower) UL = 'L';
            else if (Uplo == CblasUpper) UL = 'U';
            else
            {
                cblas_xerbla(2, "cblas_cher2","Illegal Uplo setting, %d\n",Uplo );
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
#endif

            FC_GLOBAL(cher2,CHER2)(F77_UL, &F77_N, alpha, X, &F77_incX,
                    Y, &F77_incY, A, &F77_lda);

        }  else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if (Uplo == CblasUpper) UL = 'L';
            else if (Uplo == CblasLower) UL = 'U';
            else
            {
                cblas_xerbla(2, "cblas_cher2","Illegal Uplo setting, %d\n", Uplo);
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
                y = malloc(n*sizeof(float));
                tx = x;
                ty = y;
                if( incX > 0 ) {
                    i = incX << 1 ;
                    tincx = 2;
                    stx= x+n;
                } else {
                    i = incX *(-2);
                    tincx = -2;
                    stx = x-2;
                    x +=(n-2);
                }

                if( incY > 0 ) {
                    j = incY << 1;
                    tincy = 2;
                    sty= y+n;
                } else {
                    j = incY *(-2);
                    tincy = -2;
                    sty = y-2;
                    y +=(n-2);
                }

                do
                {
                    *x = *xx;
                    x[1] = -xx[1];
                    x += tincx ;
                    xx += i;
                }
                while (x != stx);

                do
                {
                    *y = *yy;
                    y[1] = -yy[1];
                    y += tincy ;
                    yy += j;
                }
                while (y != sty);

                x=tx;
                y=ty;

#ifdef F77_INT
                F77_incX = 1;
                F77_incY = 1;
#else
                incx = 1;
                incy = 1;
#endif
            }  else
            {
                COPY_CONST_PTR(x,X);
                COPY_CONST_PTR(y,Y);
            }
            FC_GLOBAL(cher2,CHER2)(F77_UL, &F77_N, alpha, y, &F77_incY, x,
                    &F77_incX, A, &F77_lda);
        } else
        {
            cblas_xerbla(1, "cblas_cher2","Illegal layout setting, %d\n", layout);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
        if(X!=x)
            free(x);
        if(Y!=y)
            free(y);

        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;

    }
    return;
}
