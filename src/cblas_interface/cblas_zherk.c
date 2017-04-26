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

void cblas_zherk(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo,
                 const enum CBLAS_TRANSPOSE Trans, const int N, const int K,
                 const double alpha, const void *A, const int lda,
                 const double beta, void *C, const int ldc)
{
   char UL, TR;   
   #define F77_TR &TR  
   #define F77_UL &UL  

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldc ldc
#endif
   
   flexiblas_call_zherk[POS_CBLAS] ++;

   if ( flexiblas_zherk.call_cblas != NULL ) {
	   double te = 0, ts = 0;
	   if (__flexiblas_profile ) {
		   ts = flexiblas_wtime(); 
	   }
	   
	   void (*fn)
		  (const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo,
                 const enum CBLAS_TRANSPOSE Trans, const int N, const int K,
                 const double alpha, const void *A, const int lda,
                 const double beta, void *C, const int ldc)
		   = flexiblas_zherk.call_cblas;
	fn(Order,Uplo,Trans,N,K,alpha,A,lda,beta,C,ldc);
	if ( __flexiblas_profile ){
	   te = flexiblas_wtime(); 
	   flexiblas_time_zherk[POS_CBLAS] += (te - ts); 
	}
   } else {
	   extern int CBLAS_CallFromC;
	   extern int RowMajorStrg;
	   RowMajorStrg = 0;
	   CBLAS_CallFromC = 1;

	   if( Order == CblasColMajor )
	   {
	      if( Uplo == CblasUpper) UL='U';
	      else if ( Uplo == CblasLower ) UL='L';
	      else 
	      {
		 cblas_xerbla(2, "cblas_zherk", "Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if( Trans == CblasTrans) TR ='T';
	      else if ( Trans == CblasConjTrans ) TR='C';
	      else if ( Trans == CblasNoTrans )   TR='N';
	      else 
	      {
		 cblas_xerbla(3, "cblas_zherk", "Illegal Trans setting, %d\n", Trans);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }


	      F77_zherk(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda,
			     &beta, C, &F77_ldc);
	   } else if (Order == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      if( Uplo == CblasUpper) UL='L';
	      else if ( Uplo == CblasLower ) UL='U';
	      else 
	      {
		 cblas_xerbla(3, "cblas_zherk", "Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      if( Trans == CblasTrans) TR ='N';
	      else if ( Trans == CblasConjTrans ) TR='N';
	      else if ( Trans == CblasNoTrans )   TR='C';
	      else 
	      {
		 cblas_xerbla(3, "cblas_zherk", "Illegal Trans setting, %d\n", Trans);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }


	      F77_zherk(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda,
			&beta, C, &F77_ldc);
	   } 
	   else  cblas_xerbla(1, "cblas_zherk", "Illegal Order setting, %d\n", Order);
	   CBLAS_CallFromC = 0;
	   RowMajorStrg = 0;
   }
   return;
}
