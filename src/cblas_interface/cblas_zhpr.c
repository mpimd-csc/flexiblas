/* $Id: hooks.h 3741 2013-10-01 12:54:54Z komart $ */
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
#include "../hooks.h"

void cblas_zhpr(const enum CBLAS_ORDER order, const enum CBLAS_UPLO Uplo,
                const int N, const double alpha, const void *X,
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
#ifdef FLEXIBLAS_PROFILE
   flexiblas_call_zhpr[POS_CBLAS] ++;
#endif 

   if ( flexiblas_zhpr.call_cblas != NULL ) {
#ifdef FLEXIBLAS_PROFILE
	   double te, ts = flexiblas_wtime(); 
#endif
	   void (*fn)
		 (const enum CBLAS_ORDER order, const enum CBLAS_UPLO Uplo,
                const int N, const double alpha, const void *X,
                const int incX, void *A)
		   = flexiblas_zhpr.call_cblas;
	fn(order,Uplo,N,alpha,X,incX,A);
; 
#ifdef FLEXIBLAS_PROFILE
	   te = flexiblas_wtime(); 
	   flexiblas_time_zhpr[POS_CBLAS] += (te - ts); 
#endif
   } else {

	   
	   int n, i, tincx, incx=incX;
	   double *x=(double *)X, *xx=(double *)X, *tx, *st;

	   extern int CBLAS_CallFromC;
	   extern int RowMajorStrg;
	   RowMajorStrg = 0;
	 
	   CBLAS_CallFromC = 1;
	   if (order == CblasColMajor)
	   {
	      if (Uplo == CblasLower) UL = 'L';
	      else if (Uplo == CblasUpper) UL = 'U';
	      else 
	      {
		 cblas_xerbla(2, "cblas_zhpr","Illegal Uplo setting, %d\n",Uplo );
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      F77_zhpr(F77_UL, &F77_N, &alpha, X, &F77_incX, A);

	   }  else if (order == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      if (Uplo == CblasUpper) UL = 'L';
	      else if (Uplo == CblasLower) UL = 'U';
	      else 
	      {
		 cblas_xerbla(2, "cblas_zhpr","Illegal Uplo setting, %d\n", Uplo);
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
	      else x = (double *) X;

	      F77_zhpr(F77_UL, &F77_N, &alpha, x, &F77_incX, A);

	   } else 
	   {
	      cblas_xerbla(1, "cblas_zhpr","Illegal Order setting, %d\n", order);
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
