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

void cblas_chemv(const CBLAS_LAYOUT layout,
        const CBLAS_UPLO Uplo, const int N,
        const void *alpha, const void *A, const int lda,
        const void *X, const int incX, const void *beta,
        void  *Y, const int incY)
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
    current_backend->blas.chemv.calls[POS_CBLAS] ++;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    if ( current_backend->blas.chemv.call_cblas != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout,
             const CBLAS_UPLO Uplo, const int N,
             const void *alpha, const void *A, const int lda,
             const void *X, const int incX, const void *beta,
             void  *Y, const int incY)
            = current_backend->blas.chemv.call_cblas;
        fn(layout,Uplo,N,alpha,A,lda,X,incX,beta,Y,incY);
    } else {
        int n=0, i=0;
#ifdef F77_INT
        F77_INT _incX = incX;
#else
        int _incX = incX;
#endif
        const float *alp= (const float *)alpha, *bet = (const float *)beta;
        float *xx;
        float ALPHA[2],BETA[2];
        int tincY, tincx;
        float *x, *y=(float *)Y, *st=0, *tx;
        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;
        COPY_CONST_PTR(xx,X);
        COPY_CONST_PTR(x,X);


        CBLAS_CallFromC = 1;
        if (layout == CblasColMajor)
        {
            if (Uplo == CblasUpper) UL = 'U';
            else if (Uplo == CblasLower) UL = 'L';
            else
            {
                cblas_xerbla(2, "cblas_chemv","Illegal Uplo setting, %d\n",Uplo );
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
#endif
            FC_GLOBAL(chemv,CHEMV)(F77_UL, &F77_N, alpha, A, &F77_lda, X, &F77_incX,
                    beta, Y, &F77_incY);
        }
        else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            ALPHA[0]= *alp;
            ALPHA[1]= -alp[1];
            BETA[0]= *bet;
            BETA[1]= -bet[1];

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



                _incX = 1;

                if(incY > 0)
                    tincY = incY;
                else
                    tincY = -incY;
                y++;

                i = tincY << 1;
                n = i * N ;
                st = y + n;
                do {
                    *y = -(*y);
                    y += i;
                } while(y != st);
                y -= n;
            }  else {
                COPY_CONST_PTR(x,X);
            }


            if (Uplo == CblasUpper) UL = 'L';
            else if (Uplo == CblasLower) UL = 'U';
            else
            {
                cblas_xerbla(2, "cblas_chemv","Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            FC_GLOBAL(chemv,CHEMV)(F77_UL, &F77_N, ALPHA, A, &F77_lda, x, &_incX,
                    BETA, Y, &F77_incY);
        }
        else
        {
            cblas_xerbla(1, "cblas_chemv","Illegal Order setting, %d\n", layout);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
        if ( layout == CblasRowMajor )
        {
            RowMajorStrg = 1;
            if ( X != x )
                free(x);
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
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;


    }
    return;
}
