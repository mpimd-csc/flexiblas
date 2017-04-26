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

void cblas_stbmv(const enum CBLAS_ORDER order, const enum CBLAS_UPLO Uplo,
                 const enum CBLAS_TRANSPOSE TransA, const enum CBLAS_DIAG Diag,
                 const int N, const int K, const float  *A, const int lda,
                 float  *X, const int incX)
{
   char TA;
   char UL;
   char DI;
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI   
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_K=K, F77_incX=incX;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incX
#endif
#ifdef FLEXIBLAS_PROFILE
   flexiblas_call_stbmv[POS_CBLAS] ++;
#endif 

   if ( flexiblas_stbmv.call_cblas != NULL ) {
#ifdef FLEXIBLAS_PROFILE
	   double te, ts = flexiblas_wtime(); 
#endif
	   void (*fn)
		  (const enum CBLAS_ORDER order, const enum CBLAS_UPLO Uplo,
                 const enum CBLAS_TRANSPOSE TransA, const enum CBLAS_DIAG Diag,
                 const int N, const int K, const float  *A, const int lda,
                 float  *X, const int incX)
		   = flexiblas_stbmv.call_cblas;
	fn(order,Uplo,TransA,Diag,N,K,A,lda,X,incX);
#ifdef FLEXIBLAS_PROFILE
	   te = flexiblas_wtime(); 
	   flexiblas_time_stbmv[POS_CBLAS] += (te - ts); 
#endif
   } else {

	   extern int CBLAS_CallFromC;
	   extern int RowMajorStrg;
	   RowMajorStrg = 0;

	   CBLAS_CallFromC = 1;
	   if (order == CblasColMajor)
	   {
	      if (Uplo == CblasUpper) UL = 'U';
	      else if (Uplo == CblasLower) UL = 'L';
	      else 
	      {
		 cblas_xerbla(2, "cblas_stbmv","Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      if (TransA == CblasNoTrans) TA = 'N';
	      else if (TransA == CblasTrans) TA = 'T';
	      else if (TransA == CblasConjTrans) TA = 'C';
	      else 
	      {
		 cblas_xerbla(3, "cblas_stbmv","Illegal TransA setting, %d\n", TransA);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      if (Diag == CblasUnit) DI = 'U';
	      else if (Diag == CblasNonUnit) DI = 'N';
	      else 
	      {
		 cblas_xerbla(4, "cblas_stbmv","Illegal Diag setting, %d\n", Diag);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      #ifdef F77_CHAR
		 F77_UL = C2F_CHAR(&UL);
		 F77_TA = C2F_CHAR(&TA);
		 F77_DI = C2F_CHAR(&DI);
	      #endif
	      F77_stbmv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
			      &F77_incX);
	   }
	   else if (order == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      if (Uplo == CblasUpper) UL = 'L';
	      else if (Uplo == CblasLower) UL = 'U';
	      else 
	      {
		 cblas_xerbla(2, "cblas_stbmv","Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if (TransA == CblasNoTrans) TA = 'T';
	      else if (TransA == CblasTrans) TA = 'N';
	      else if (TransA == CblasConjTrans) TA = 'N';
	      else 
	      {
		 cblas_xerbla(3, "cblas_stbmv","Illegal TransA setting, %d\n", TransA);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if (Diag == CblasUnit) DI = 'U';
	      else if (Diag == CblasNonUnit) DI = 'N';
	      else 
	      {
		 cblas_xerbla(4, "cblas_stbmv","Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      #ifdef F77_CHAR
		 F77_UL = C2F_CHAR(&UL);
		 F77_TA = C2F_CHAR(&TA);
		 F77_DI = C2F_CHAR(&DI);
	      #endif

	      F77_stbmv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
			      &F77_incX);

	   }
	   else cblas_xerbla(1, "cblas_stbmv", "Illegal Order setting, %d\n", order);
	   CBLAS_CallFromC = 0;
	   RowMajorStrg = 0;
   }
   return; 
}
