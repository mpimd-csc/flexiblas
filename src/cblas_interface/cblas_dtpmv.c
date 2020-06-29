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


void cblas_dtpmv(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const int N, const double  *Ap, double  *X, const int incX)
{
   char TA;
   char UL;
   char DI;
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   current_backend->blas.dtpmv.calls[POS_CBLAS] ++;

   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.dtpmv.call_cblas != NULL ) {
	   void (*fn)
		 (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const int N, const double  *Ap, double  *X, const int incX)
		   = current_backend->blas.dtpmv.call_cblas;
		fn(layout,Uplo,TransA,Diag,N,Ap,X,incX);
   } else {

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
		 cblas_xerbla(2, "cblas_dtpmv","Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      if (TransA == CblasNoTrans) TA = 'N';
	      else if (TransA == CblasTrans) TA = 'T';
	      else if (TransA == CblasConjTrans) TA = 'C';
	      else
	      {
		 cblas_xerbla(3, "cblas_dtpmv","Illegal TransA setting, %d\n", TransA);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      if (Diag == CblasUnit) DI = 'U';
	      else if (Diag == CblasNonUnit) DI = 'N';
	      else
	      {
		 cblas_xerbla(4, "cblas_dtpmv","Illegal Diag setting, %d\n", Diag);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      #ifdef F77_CHAR
		 F77_UL = C2F_CHAR(&UL);
		 F77_TA = C2F_CHAR(&TA);
		 F77_DI = C2F_CHAR(&DI);
	      #endif
	      FC_GLOBAL(dtpmv,DTPMV)( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X, &F77_incX);
	   }
	   else if (layout == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      if (Uplo == CblasUpper) UL = 'L';
	      else if (Uplo == CblasLower) UL = 'U';
	      else
	      {
		 cblas_xerbla(2, "cblas_dtpmv","Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if (TransA == CblasNoTrans) TA = 'T';
	      else if (TransA == CblasTrans) TA = 'N';
	      else if (TransA == CblasConjTrans) TA = 'N';
	      else
	      {
		 cblas_xerbla(3, "cblas_dtpmv","Illegal TransA setting, %d\n", TransA);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if (Diag == CblasUnit) DI = 'U';
	      else if (Diag == CblasNonUnit) DI = 'N';
	      else
	      {
		 cblas_xerbla(4, "cblas_dtpmv","Illegal Diag setting, %d\n", Diag);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      #ifdef F77_CHAR
		 F77_UL = C2F_CHAR(&UL);
		 F77_TA = C2F_CHAR(&TA);
		 F77_DI = C2F_CHAR(&DI);
	      #endif

	      FC_GLOBAL(dtpmv,DTPMV)( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X,&F77_incX);
	   }
	   else cblas_xerbla(1, "cblas_dtpmv", "Illegal layout setting, %d\n", layout);
	   CBLAS_CallFromC = 0;
	   RowMajorStrg = 0;
   }
   return;
}
