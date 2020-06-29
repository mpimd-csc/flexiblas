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




void cblas_sspr(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const float alpha, const float *X,
                const int incX, float *Ap)
{
   char UL;
   #define F77_UL &UL

#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   current_backend->blas.sspr.calls[POS_CBLAS] ++;

   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.sspr.call_cblas != NULL ) {
	   void (*fn)
		(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const int N, const float alpha, const float *X,
                const int incX, float *Ap)
		   = current_backend->blas.sspr.call_cblas;
	fn(layout,Uplo,N,alpha,X,incX,Ap);
   } else {
	   extern int CBLAS_CallFromC;
	   extern int RowMajorStrg;
	   RowMajorStrg = 0;
	   CBLAS_CallFromC = 1;
	   if (layout == CblasColMajor)
	   {
	      if (Uplo == CblasLower) UL = 'L';
	      else if (Uplo == CblasUpper) UL = 'U';
	      else
	      {
		 cblas_xerbla(2, "cblas_sspr","Illegal Uplo setting, %d\n",Uplo );
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      #ifdef F77_CHAR
		 F77_UL = C2F_CHAR(&UL);
	      #endif

	      FC_GLOBAL(sspr,SSPR)(F77_UL, &F77_N, &alpha, X, &F77_incX, Ap);

	   }  else if (layout == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      if (Uplo == CblasLower) UL = 'U';
	      else if (Uplo == CblasUpper) UL = 'L';
	      else
	      {
		 cblas_xerbla(2, "cblas_sspr","Illegal Uplo setting, %d\n",Uplo );
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      #ifdef F77_CHAR
		 F77_UL = C2F_CHAR(&UL);
	      #endif
	      FC_GLOBAL(sspr,SSPR)(F77_UL, &F77_N, &alpha, X, &F77_incX, Ap);
	   } else cblas_xerbla(1, "cblas_sspr", "Illegal layout setting, %d\n", layout);
	   CBLAS_CallFromC = 0;
	   RowMajorStrg = 0;
   }
   return;
}
