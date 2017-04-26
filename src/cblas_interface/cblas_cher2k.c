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


void cblas_cher2k(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo,
                  const enum CBLAS_TRANSPOSE Trans, const int N, const int K,
                  const void *alpha, const void *A, const int lda,
                  const void *B, const int ldb, const float beta,
                  void *C, const int ldc)
{
   char UL, TR;   
   #define F77_TR &TR  
   #define F77_UL &UL  

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif
#ifdef FLEXIBLAS_PROFILE
   flexiblas_call_cher2k[POS_CBLAS] ++;
#endif 

   if ( flexiblas_cher2k.call_cblas != NULL ) {
#ifdef FLEXIBLAS_PROFILE
	   double te, ts = flexiblas_wtime(); 
#endif
	   void (*fn)
		  (const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo,
                  const enum CBLAS_TRANSPOSE Trans, const int N, const int K,
                  const void *alpha, const void *A, const int lda,
                  const void *B, const int ldb, const float beta,
                  void *C, const int ldc)
		   = flexiblas_cher2k.call_cblas;
	fn(Order,Uplo,Trans,N,K,alpha,A,lda,B,ldb,beta,C,ldc);
#ifdef FLEXIBLAS_PROFILE
	   te = flexiblas_wtime(); 
	   flexiblas_time_cher2k[POS_CBLAS] += (te - ts); 
#endif
   } else {

	   extern int CBLAS_CallFromC;
	   extern int RowMajorStrg;
	   float ALPHA[2]; 
	   const float *alp=(float *)alpha;

	   CBLAS_CallFromC = 1;
	   RowMajorStrg = 0;

	   if( Order == CblasColMajor )
	   {

	      if( Uplo == CblasUpper) UL='U';
	      else if ( Uplo == CblasLower ) UL='L';
	      else 
	      {
		 cblas_xerbla(2, "cblas_cher2k", "Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if( Trans == CblasTrans) TR ='T';
	      else if ( Trans == CblasConjTrans ) TR='C';
	      else if ( Trans == CblasNoTrans )   TR='N';
	      else 
	      {
		 cblas_xerbla(3, "cblas_cher2k", "Illegal Trans setting, %d\n", Trans);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      F77_cher2k(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
	   } else if (Order == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      
	      if( Uplo == CblasUpper) UL='L';
	      else if ( Uplo == CblasLower ) UL='U';
	      else 
	      {
		 cblas_xerbla(2, "cblas_cher2k", "Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      if( Trans == CblasTrans) TR ='N';
	      else if ( Trans == CblasConjTrans ) TR='N';
	      else if ( Trans == CblasNoTrans )   TR='C';
	      else 
	      {
		 cblas_xerbla(3, "cblas_cher2k", "Illegal Trans setting, %d\n", Trans);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      ALPHA[0]= *alp;
	      ALPHA[1]= -alp[1];
	      F77_cher2k(F77_UL,F77_TR, &F77_N, &F77_K, ALPHA, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
	   } 
	   else  cblas_xerbla(1, "cblas_cher2k", "Illegal Order setting, %d\n", Order);
	   CBLAS_CallFromC = 0;
	   RowMajorStrg = 0;
   }
   return;
}
