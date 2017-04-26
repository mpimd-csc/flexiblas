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


void cblas_dgemm(const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE TransA,
                 const enum CBLAS_TRANSPOSE TransB, const int M, const int N,
                 const int K, const double alpha, const double  *A,
                 const int lda, const double  *B, const int ldb,
                 const double beta, double  *C, const int ldc)
{
   char TA, TB;   
   #define F77_TA &TA  
   #define F77_TB &TB  

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif
#ifdef FLEXIBLAS_PROFILE
   flexiblas_call_dgemm[POS_CBLAS] ++;
#endif 

   if ( flexiblas_dgemm.call_cblas != NULL ) {
#ifdef FLEXIBLAS_PROFILE
	   double te, ts = flexiblas_wtime(); 
#endif
	   void (*fn)
		  (const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE TransA,
                 const enum CBLAS_TRANSPOSE TransB, const int M, const int N,
                 const int K, const double alpha, const double  *A,
                 const int lda, const double  *B, const int ldb,
                 const double beta, double  *C, const int ldc) 
		   = flexiblas_dgemm.call_cblas;
	   fn(Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc); 
#ifdef FLEXIBLAS_PROFILE
	   te = flexiblas_wtime(); 
	   flexiblas_time_dgemm[POS_CBLAS] += (te - ts); 
#endif
   } else {

	   extern int CBLAS_CallFromC;
	   extern int RowMajorStrg;
	   RowMajorStrg = 0;
	   CBLAS_CallFromC = 1;

	   if( Order == CblasColMajor )
	   {
	      if(TransA == CblasTrans) TA='T';
	      else if ( TransA == CblasConjTrans ) TA='C';
	      else if ( TransA == CblasNoTrans )   TA='N';
	      else 
	      {
		 cblas_xerbla(2, "cblas_dgemm","Illegal TransA setting, %d\n", TransA);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if(TransB == CblasTrans) TB='T';
	      else if ( TransB == CblasConjTrans ) TB='C';
	      else if ( TransB == CblasNoTrans )   TB='N';
	      else 
	      {
		 cblas_xerbla(3, "cblas_dgemm","Illegal TransB setting, %d\n", TransB);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      F77_dgemm(F77_TA, F77_TB, &F77_M, &F77_N, &F77_K, &alpha, A,  &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
	   } else if (Order == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      if(TransA == CblasTrans) TB='T';
	      else if ( TransA == CblasConjTrans ) TB='C';
	      else if ( TransA == CblasNoTrans )   TB='N';
	      else 
	      {
		 cblas_xerbla(2, "cblas_dgemm","Illegal TransA setting, %d\n", TransA);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      if(TransB == CblasTrans) TA='T';
	      else if ( TransB == CblasConjTrans ) TA='C';
	      else if ( TransB == CblasNoTrans )   TA='N';
	      else 
	      {
		 cblas_xerbla(2, "cblas_dgemm","Illegal TransB setting, %d\n", TransB);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      F77_dgemm(F77_TA, F77_TB, &F77_N, &F77_M, &F77_K, &alpha, B,  &F77_ldb, A, &F77_lda, &beta, C, &F77_ldc);
	   } 
	   else  cblas_xerbla(1, "cblas_dgemm", "Illegal Order setting, %d\n", Order);
	   CBLAS_CallFromC = 0;
	   RowMajorStrg = 0;
   }
   return;
}

