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



void cblas_dger(const enum CBLAS_ORDER order, const int M, const int N,
                const double alpha, const double  *X, const int incX,
                const double  *Y, const int incY, double  *A, const int lda)
{
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_M M
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
   #define F77_lda lda
#endif
#ifdef FLEXIBLAS_PROFILE
   flexiblas_call_dger[POS_CBLAS] ++;
#endif 

   if ( flexiblas_dger.call_cblas != NULL ) {
#ifdef FLEXIBLAS_PROFILE
	   double te, ts = flexiblas_wtime(); 
#endif
	   void (*fn)(const enum CBLAS_ORDER order, const int M, const int N,
                const double alpha, const double  *X, const int incX,
                const double  *Y, const int incY, double  *A, const int lda)
                = flexiblas_dger.call_cblas;
	fn(order,M,N,alpha,X,incX,Y,incY,A,lda);
#ifdef FLEXIBLAS_PROFILE
	   te = flexiblas_wtime(); 
	   flexiblas_time_dger[POS_CBLAS] += (te - ts); 
#endif
   } else {

	   extern int CBLAS_CallFromC;
	   extern int RowMajorStrg;
	   RowMajorStrg = 0;

	   CBLAS_CallFromC = 1;
	   if (order == CblasColMajor)
	   {
	      F77_dger( &F77_M, &F77_N, &alpha, X, &F77_incX, Y, &F77_incY, A, 
			      &F77_lda);
	   }
	   else if (order == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      F77_dger( &F77_N, &F77_M ,&alpha, Y, &F77_incY, X, &F77_incX, A, 
			      &F77_lda);

	   }
	   else cblas_xerbla(1, "cblas_dger", "Illegal Order setting, %d\n", order);
	   CBLAS_CallFromC = 0;
	   RowMajorStrg = 0;
   }
   return;
}
