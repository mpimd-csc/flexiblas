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

void cblas_zhpr2(const enum CBLAS_ORDER order, const enum CBLAS_UPLO Uplo,
                      const int N,const void *alpha, const void *X, 
                      const int incX,const void *Y, const int incY, void *Ap)

{
   char UL;
   #define F77_UL &UL

#ifdef F77_INT
   F77_INT F77_N=N,  F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incx
   #define F77_incY incy
#endif
   flexiblas_call_zhpr2[POS_CBLAS] ++;

   if ( flexiblas_zhpr2.call_cblas != NULL ) {
	   double ts = 0, te = 0;
	   if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
	   }
	   void (*fn)
		  (const enum CBLAS_ORDER order, const enum CBLAS_UPLO Uplo,
                      const int N,const void *alpha, const void *X, 
                      const int incX,const void *Y, const int incY, void *Ap)
		   = flexiblas_zhpr2.call_cblas;
	fn(order,Uplo,N,alpha,X,incX,Y,incY,Ap);
	if ( __flexiblas_profile ){
	   te = flexiblas_wtime(); 
	   flexiblas_time_zhpr2[POS_CBLAS] += (te - ts); 
	}
   } else {

	   int n, i, j, tincx, tincy; 
#ifdef F77_INT
	   F77_incX=incX;
	   F77_incY=incY;

#else 
	   int incx = incX; 
	   int incy = incY; 
#endif
	   double *x=(double *)X, *xx=(double *)X, *y=(double *)Y,
		 *yy=(double *)Y, *tx, *ty, *stx, *sty;

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
		 cblas_xerbla(2, "cblas_zhpr2","Illegal Uplo setting, %d\n",Uplo );
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      F77_zhpr2(F77_UL, &F77_N, alpha, X, &F77_incX, Y, &F77_incY, Ap);

	   }  else if (order == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      if (Uplo == CblasUpper) UL = 'L';
	      else if (Uplo == CblasLower) UL = 'U';
	      else 
	      {
		 cblas_xerbla(2, "cblas_zhpr2","Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      if (N > 0)
	      {
		 n = N << 1;
		 x = malloc(n*sizeof(double));
		 y = malloc(n*sizeof(double));
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
		 x = (double *) X;
		 y = (void  *) Y;
	      }
	      F77_zhpr2(F77_UL, &F77_N, alpha, y, &F77_incY, x, &F77_incX, Ap);
	   } else 
	   {
	      cblas_xerbla(1, "cblas_zhpr2","Illegal Order setting, %d\n", order);
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
