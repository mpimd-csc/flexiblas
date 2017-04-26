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

void cblas_ztrmm(const enum CBLAS_ORDER Order, const enum CBLAS_SIDE Side,
                 const enum CBLAS_UPLO Uplo, const  enum CBLAS_TRANSPOSE TransA,
                 const enum CBLAS_DIAG Diag, const int M, const int N,
                 const void *alpha, const void  *A, const int lda,
                 void  *B, const int ldb)
{
   char UL, TA, SD, DI;   
   #define F77_TA &TA  
   #define F77_UL &UL  
   #define F77_SD &SD
   #define F77_DI &DI

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
#endif
#ifdef FLEXIBLAS_PROFILE
   flexiblas_call_ztrmm[POS_CBLAS] ++;
#endif 

   if ( flexiblas_ztrmm.call_cblas != NULL ) {
#ifdef FLEXIBLAS_PROFILE
	   double te, ts = flexiblas_wtime(); 
#endif
	   void (*fn)
		 (const enum CBLAS_ORDER Order, const enum CBLAS_SIDE Side,
                 const enum CBLAS_UPLO Uplo, const  enum CBLAS_TRANSPOSE TransA,
                 const enum CBLAS_DIAG Diag, const int M, const int N,
                 const void *alpha, const void  *A, const int lda,
                 void  *B, const int ldb)
		   = flexiblas_ztrmm.call_cblas;
	fn(Order,Side,Uplo,TransA,Diag,M,N,alpha,A,lda,B,ldb);
#ifdef FLEXIBLAS_PROFILE
	   te = flexiblas_wtime(); 
	   flexiblas_time_ztrmm[POS_CBLAS] += (te - ts); 
#endif
   } else {

	   extern int CBLAS_CallFromC;
	   extern int RowMajorStrg;
	   RowMajorStrg = 0;
	   CBLAS_CallFromC = 1;

	   if( Order == CblasColMajor )
	   {
	      if( Side == CblasRight ) SD='R';
	      else if ( Side == CblasLeft ) SD='L';
	      else 
	      {
		 cblas_xerbla(2, "cblas_ztrmm", "Illegal Side setting, %d\n", Side);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      if( Uplo == CblasUpper ) UL='U';
	      else if ( Uplo == CblasLower ) UL='L';
	      else 
	      {
		 cblas_xerbla(3, "cblas_ztrmm", "Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if( TransA == CblasTrans ) TA ='T';
	      else if ( TransA == CblasConjTrans ) TA='C';
	      else if ( TransA == CblasNoTrans )   TA='N';
	      else 
	      {
		 cblas_xerbla(4, "cblas_ztrmm", "Illegal Trans setting, %d\n", TransA);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if( Diag == CblasUnit ) DI='U';
	      else if ( Diag == CblasNonUnit ) DI='N';
	      else cblas_xerbla(5, "cblas_ztrmm", 
			       "Illegal Diag setting, %d\n", Diag);

	      F77_ztrmm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_M, &F77_N, alpha, A, &F77_lda, B, &F77_ldb);
	   } else if (Order == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      if( Side == CblasRight ) SD='L';
	      else if ( Side == CblasLeft ) SD='R';
	      else 
	      {
		 cblas_xerbla(2, "cblas_ztrmm", "Illegal Side setting, %d\n", Side);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if( Uplo == CblasUpper ) UL='L';
	      else if ( Uplo == CblasLower ) UL='U';
	      else 
	      {
		 cblas_xerbla(3, "cblas_ztrmm", "Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if( TransA == CblasTrans ) TA ='T';
	      else if ( TransA == CblasConjTrans ) TA='C';
	      else if ( TransA == CblasNoTrans )   TA='N';
	      else 
	      {
		 cblas_xerbla(4, "cblas_ztrmm", "Illegal Trans setting, %d\n", TransA);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if( Diag == CblasUnit ) DI='U';
	      else if ( Diag == CblasNonUnit ) DI='N';
	      else 
	      {
		 cblas_xerbla(5, "cblas_ztrmm", "Illegal Diag setting, %d\n", Diag);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      F77_ztrmm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_N, &F77_M, alpha, A, &F77_lda, B, &F77_ldb);
	   } 
	   else  cblas_xerbla(1, "cblas_ztrmm", "Illegal Order setting, %d\n", Order);
	   CBLAS_CallFromC = 0;
	   RowMajorStrg = 0;
   }
   return;
}
