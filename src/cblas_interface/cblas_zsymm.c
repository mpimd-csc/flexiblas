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


void cblas_zsymm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const int M, const int N,
                 const void *alpha, const void  *A, const int lda,
                 const void  *B, const int ldb, const void *beta,
                 void  *C, const int ldc)
{
   char SD, UL;
   #define F77_SD &SD
   #define F77_UL &UL

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif
   current_backend->blas.zsymm.calls[POS_CBLAS] ++;

   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.zsymm.call_cblas != NULL ) {
	   void (*fn)
		 (const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const int M, const int N,
                 const void *alpha, const void  *A, const int lda,
                 const void  *B, const int ldb, const void *beta,
                 void  *C, const int ldc)
		   = current_backend->blas.zsymm.call_cblas;
	fn(layout,Side,Uplo,M,N,alpha,A,lda,B,ldb,beta,C,ldc);
   } else {

	   extern int CBLAS_CallFromC;
	   extern int RowMajorStrg;
	   RowMajorStrg = 0;
	   CBLAS_CallFromC = 1;

	   if( layout == CblasColMajor )
	   {
	      if( Side == CblasRight) SD='R';
	      else if ( Side == CblasLeft ) SD='L';
	      else
	      {
		 cblas_xerbla(2, "cblas_zsymm", "Illegal Side setting, %d\n", Side);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if( Uplo == CblasUpper) UL='U';
	      else if ( Uplo == CblasLower ) UL='L';
	      else
	      {
		 cblas_xerbla(3, "cblas_zsymm", "Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      FC_GLOBAL(zsymm,ZSYMM)(F77_SD, F77_UL, &F77_M, &F77_N, alpha, A, &F77_lda,
			      B, &F77_ldb, beta, C, &F77_ldc);
	   } else if (layout == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      if( Side == CblasRight) SD='L';
	      else if ( Side == CblasLeft ) SD='R';
	      else
	      {
		 cblas_xerbla(2, "cblas_zsymm", "Illegal Side setting, %d\n", Side);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }

	      if( Uplo == CblasUpper) UL='L';
	      else if ( Uplo == CblasLower ) UL='U';
	      else
	      {
		 cblas_xerbla(3, "cblas_zsymm", "Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }


	      FC_GLOBAL(zsymm,ZSYMM)(F77_SD, F77_UL, &F77_N, &F77_M, alpha, A, &F77_lda,
			     B, &F77_ldb, beta, C, &F77_ldc);
	   }
	   else cblas_xerbla(1, "cblas_zsymm", "Illegal layout setting, %d\n", layout);
	   CBLAS_CallFromC = 0;
	   RowMajorStrg = 0;
   }
   return;
}
