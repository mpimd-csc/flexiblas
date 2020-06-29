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

void cblas_zher(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const double alpha, const void *X, const int incX
                ,void *A, const int lda)
{
   char UL;
   #define F77_UL &UL

#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incx
#endif
   current_backend->blas.zher.calls[POS_CBLAS] ++;

   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.zher.call_cblas != NULL ) {
	   void (*fn)
		  (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const double alpha, const void *X, const int incX
                ,void *A, const int lda)
		   = current_backend->blas.zher.call_cblas;
	fn(layout,Uplo,N,alpha,X,incX,A,lda);
   } else {
	   int n, i, tincx;
#ifdef F77_INT
	   F77_incX=incX;
#else
	   int incx = incX;
#endif
	   double *x, *xx, *tx, *st;


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
		 cblas_xerbla(2, "cblas_zher","Illegal Uplo setting, %d\n",Uplo );
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      FC_GLOBAL(zher,ZHER)(F77_UL, &F77_N, &alpha, X, &F77_incX, A, &F77_lda);

	   }  else if (layout == CblasRowMajor)
	   {
	      RowMajorStrg = 1;

          if (Uplo == CblasUpper) UL = 'L';
	      else if (Uplo == CblasLower) UL = 'U';
	      else
	      {
		 cblas_xerbla(2, "cblas_zher","Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      if (N > 0)
	      {
		 n = N << 1;
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

		 #ifdef F77_INT
		   F77_incX = 1;
		 #else
		   incx = 1;
		 #endif
	      }
	      else {
            COPY_CONST_PTR(x,X);
          }
	      FC_GLOBAL(zher,ZHER)(F77_UL, &F77_N, &alpha, x, &F77_incX, A, &F77_lda);
	   } else
	   {
	      cblas_xerbla(1, "cblas_zher","Illegal layout setting, %d\n", layout);
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
