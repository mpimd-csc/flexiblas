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

void cblas_zhbmv(const enum CBLAS_ORDER order,
                 const enum CBLAS_UPLO Uplo,const int N,const int K,
                 const void *alpha, const void  *A, const int lda,
                 const void  *X, const int incX, const void *beta,
                 void  *Y, const int incY)
{
   char UL;
   #define F77_UL &UL   
#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incx
   #define F77_incY incY
#endif
   current_backend->blas.zhbmv.calls[POS_CBLAS] ++;

   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.zhbmv.call_cblas != NULL ) {
	   double te = 0 , ts = 0;
	   if ( __flexiblas_profile ){ 
		   ts = flexiblas_wtime(); 
	   }
	   void (*fn)
		  (const enum CBLAS_ORDER order,
                 const enum CBLAS_UPLO Uplo,const int N,const int K,
                 const void *alpha, const void  *A, const int lda,
                 const void  *X, const int incX, const void *beta,
                 void  *Y, const int incY)
		   = current_backend->blas.zhbmv.call_cblas;
	fn(order,Uplo,N,K,alpha,A,lda,X,incX,beta,Y,incY);
	if ( __flexiblas_profile ) {
	   te = flexiblas_wtime(); 
	   current_backend->blas.zhbmv.timings[POS_CBLAS] += (te - ts); 
	}
   } else {

	   int n, i=0;
#ifdef F77_INT
	   F77_incX=incX;
#else 
	   int incx = incX; 
#endif
	   const double *xx= (double *)X, *alp= (double *)alpha, *bet = (double *)beta;
	   double ALPHA[2],BETA[2];
	   int tincY, tincx;
	   double *x=(double *)X, *y=(double *)Y, *st=0, *tx;
	   extern int CBLAS_CallFromC;
	   extern int RowMajorStrg;
	   RowMajorStrg = 0;

	   CBLAS_CallFromC = 1;
	   if (order == CblasColMajor)
	   {
	      if (Uplo == CblasLower) UL = 'L';
	      else if (Uplo == CblasUpper) UL = 'U';
	      else 
	      {
		 cblas_xerbla(2, "cblas_zhbmv","Illegal Uplo setting, %d\n",Uplo );
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      F77_zhbmv(F77_UL, &F77_N, &F77_K, alpha, A, &F77_lda, X,  
			     &F77_incX, beta, Y, &F77_incY);
	   }
	   else if (order == CblasRowMajor)
	   {
	      RowMajorStrg = 1;
	      ALPHA[0]= *alp;
	      ALPHA[1]= -alp[1];
	      BETA[0]= *bet;
	      BETA[1]= -bet[1];

	      if (N > 0)
	      {
		 n = N << 1;
		 x = malloc(n*sizeof(double));
	 
		 tx = x;
		 if( incX > 0 ) {
		   i = incX << 1 ;
		   tincx = 2;
		   st= x+n;
		 } else {
		   i = incX *(-2);
		   tincx = -2;
		   st = x-2;
		   x +=(n-2);
		 }

		 do
		 {
		   *x = *xx;
		   x[1] = -xx[1];
		   x += tincx ;
		   xx += i;
		 }
		 while (x != st);
		 x=tx;


		 #ifdef F77_INT
		    F77_incX = 1;
		 #else
		    incx = 1;
		 #endif
	 
		 if(incY > 0)
		   tincY = incY;
		 else
		   tincY = -incY;
		 y++;

		 i = tincY << 1;
		 n = i * N ;
		 st = y + n;
		 do {
		    *y = -(*y);
		    y += i;
		 } while(y != st);
		 y -= n;
	      }  else
		 x = (double *) X; 

	      if (Uplo == CblasUpper) UL = 'L';
	      else if (Uplo == CblasLower) UL = 'U';
	      else 
	      {
		 cblas_xerbla(2, "cblas_zhbmv","Illegal Uplo setting, %d\n", Uplo);
		 CBLAS_CallFromC = 0;
		 RowMajorStrg = 0;
		 return;
	      }
	      F77_zhbmv(F77_UL, &F77_N, &F77_K, ALPHA, 
			     A ,&F77_lda, x,&F77_incX, BETA, Y, &F77_incY);
	   }
	   else 
	   {
	      cblas_xerbla(1, "cblas_zhbmv","Illegal Order setting, %d\n", order);
	      CBLAS_CallFromC = 0;
	      RowMajorStrg = 0;
	      return;
	   }
	   if ( order == CblasRowMajor )
	   {
	      RowMajorStrg = 1;
	      if(X!=x)
		 free(x);
	      if (N > 0)
	      {
		 do
		 {
		    *y = -(*y);
		    y += i;
		 }
		 while (y != st);
	      }
	   }
	   CBLAS_CallFromC = 0;
	   RowMajorStrg = 0;
   }
   return;
}
