/* $Id: flexiblas.h 3741 2013-10-01 12:54:54Z komart $ */
/* 
   Copyright (C) 2013  Martin Köhler, koehlerm@mpi-magdeburg.mpg.de

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
   */
#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"

void cblas_zgemv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        const void  *X, const int incX, const void *beta,
        void  *Y, const int incY)
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
#endif

    current_backend->blas.zgemv.calls[POS_CBLAS] ++;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    if ( current_backend->blas.zgemv.call_cblas != NULL ) {
        double te = 0 , ts = 0;
        if ( __flexiblas_profile ) {
            ts    = flexiblas_wtime(); 
        }

        void (*fn)
            (const CBLAS_LAYOUT layout,
             const CBLAS_TRANSPOSE TransA, const int M, const int N,
             const void *alpha, const void  *A, const int lda,
             const void  *X, const int incX, const void *beta,
             void  *Y, const int incY)
            = current_backend->blas.zgemv.call_cblas;
        fn(layout,TransA,M,N,alpha,A,lda,X,incX,beta,Y,incY);
        if ( __flexiblas_profile ) {
            te = flexiblas_wtime(); 
            current_backend->blas.zgemv.timings[POS_CBLAS] += (te - ts); 
        }
    } else {
        int n=0, i=0; 
#ifdef F77_INT
        F77_incX=incX;
#else 
        int incx = incX; 
#endif


        const double *xx= (const double *)X;
        double ALPHA[2],BETA[2];
        int tincY, tincx;
        double *x, *y=(double *)Y, *st=0, *tx=0;
        const double *stx = x;
        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;

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
            FC_GLOBAL(zgemv,ZGEMV)(F77_TA, &F77_M, &F77_N, alpha, A, &F77_lda, X, &F77_incX, 
                    beta, Y, &F77_incY);
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
            if (TransA == CblasConjTrans)
                FC_GLOBAL(zgemv,ZGEMV)(F77_TA, &F77_N, &F77_M, ALPHA, A, &F77_lda, stx,
                        &F77_incX, BETA, Y, &F77_incY);
            else
                FC_GLOBAL(zgemv,ZGEMV)(F77_TA, &F77_N, &F77_M, alpha, A, &F77_lda, x,
                        &F77_incX, beta, Y, &F77_incY);

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
