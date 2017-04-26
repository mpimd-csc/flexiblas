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


void cblas_sgemv(const enum CBLAS_ORDER order,
                 const enum CBLAS_TRANSPOSE TransA, const int M, const int N,
                 const float alpha, const float  *A, const int lda,
                 const float  *X, const int incX, const float beta,
                 float  *Y, const int incY)
{
   char TA;
   #define F77_TA &TA   
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
   #define F77_incY incY
#endif
   current_backend->blas.sgemv.calls[POS_CBLAS] ++;

   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.sgemv.call_cblas != NULL ) {
	   float te = 0, ts = 0;
	   if ( __flexiblas_profile ) {
		   ts = flexiblas_wtime(); 
	   }
	   void (*fn)(const enum CBLAS_ORDER order,
                 const enum CBLAS_TRANSPOSE TransA, const int M, const int N,
                 const float alpha, const float  *A, const int lda,
                 const float  *X, const int incX, const float beta,
                 float  *Y, const int incY) = current_backend->blas.sgemv.call_cblas;
	   fn ( order, TransA, M, N, alpha, A, lda, X, incX, beta, Y,incY); 
	   if ( __flexiblas_profile ){
		   te = flexiblas_wtime(); 
		   current_backend->blas.sgemv.timings[POS_CBLAS] += (te - ts); 
	   }
   } else {
	   extern int CBLAS_CallFromC;
	   extern int RowMajorStrg;
	   RowMajorStrg = 0;

	   CBLAS_CallFromC = 1;
	   if (order == CblasColMajor)
	   {
	      if (TransA == CblasNoTrans) TA = 'N';
	      else if (TransA == CblasTrans) TA = 'T';
	      else if (TransA == CblasConjTrans) TA = 'C';
	      else 
	      {
		 cblas_xerbla(2, "cblas_sgemv","Illegal TransA setting, %d\n", TransA);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      #ifdef F77_CHAR
		 F77_TA = C2F_CHAR(&TA);
	      #endif
	      F77_sgemv(F77_TA, &F77_M, &F77_N, &alpha, A, &F77_lda, X, &F77_incX, 
			&beta, Y, &F77_incY);
	   }
	   else if (order == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      if (TransA == CblasNoTrans) TA = 'T';
	      else if (TransA == CblasTrans) TA = 'N';
	      else if (TransA == CblasConjTrans) TA = 'N';
	      else 
	      {
		 cblas_xerbla(2, "cblas_sgemv","Illegal TransA setting, %d\n", TransA);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      #ifdef F77_CHAR
		 F77_TA = C2F_CHAR(&TA);
	      #endif
	      F77_sgemv(F77_TA, &F77_N, &F77_M, &alpha, A, &F77_lda, X,
			&F77_incX, &beta, Y, &F77_incY);
	   }
	   else cblas_xerbla(1, "cblas_sgemv", "Illegal Order setting, %d\n", order);
	   CBLAS_CallFromC = 0;
	   RowMajorStrg = 0;
   }
   return;
}
