/* $Id: double.c 3754 2013-10-09 13:56:41Z komart $ */ 
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


#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
// #include <dlfcn.h>
#include <complex.h> 
#include <math.h>
#include "f77blas_interface.h"
#include "hooks.h"

/*-----------------------------------------------------------------------------
 *  Initialize the Hooks
 *-----------------------------------------------------------------------------*/
struct flexiblas_blasfn flexiblas_dasum  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dcabs1 = HOOK_INIT; 
struct flexiblas_blasfn flexiblas_dgbmv  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_drot   = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dscal  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dswap  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dsyr   = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dtpsv  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dzasum = HOOK_INIT;
struct flexiblas_blasfn flexiblas_daxpy  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dgemm  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_drotg  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dsdot  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dsymm  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dsyrk  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dtrmm  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dznrm2 = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dgemv  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_drotm  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dspmv  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dsymv  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dtbmv  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dtrmv  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dcopy  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dger   = HOOK_INIT;
struct flexiblas_blasfn flexiblas_drotmg = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dspr2  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dsyr2  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dtbsv  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dtrsm  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_ddot   = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dnrm2  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dsbmv  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dspr   = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dsyr2k = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dtpmv  = HOOK_INIT;
struct flexiblas_blasfn flexiblas_dtrsv  = HOOK_INIT;

/*-----------------------------------------------------------------------------
 *  Usecond counters for every function 
 *-----------------------------------------------------------------------------*/
double flexiblas_time_dasum  [2] = {0.0,0.0};
double flexiblas_time_dgbmv  [2] = {0.0,0.0};
double flexiblas_time_drot   [2] = {0.0,0.0};
double flexiblas_time_dscal  [2] = {0.0,0.0};
double flexiblas_time_dswap  [2] = {0.0,0.0};
double flexiblas_time_dsyr   [2] = {0.0,0.0};
double flexiblas_time_dtpsv  [2] = {0.0,0.0};
double flexiblas_time_dzasum [2] = {0.0,0.0};
double flexiblas_time_daxpy  [2] = {0.0,0.0};
double flexiblas_time_dgemm  [2] = {0.0,0.0};
double flexiblas_time_drotg  [2] = {0.0,0.0};
double flexiblas_time_dsdot  [2] = {0.0,0.0};
double flexiblas_time_dsymm  [2] = {0.0,0.0};
double flexiblas_time_dsyrk  [2] = {0.0,0.0};
double flexiblas_time_dtrmm  [2] = {0.0,0.0};
double flexiblas_time_dznrm2 [2] = {0.0,0.0};
double flexiblas_time_dgemv  [2] = {0.0,0.0};
double flexiblas_time_drotm  [2] = {0.0,0.0};
double flexiblas_time_dspmv  [2] = {0.0,0.0};
double flexiblas_time_dsymv  [2] = {0.0,0.0};
double flexiblas_time_dtbmv  [2] = {0.0,0.0};
double flexiblas_time_dtrmv  [2] = {0.0,0.0};
double flexiblas_time_dcopy  [2] = {0.0,0.0};
double flexiblas_time_dger   [2] = {0.0,0.0};
double flexiblas_time_drotmg [2] = {0.0,0.0};
double flexiblas_time_dspr2  [2] = {0.0,0.0};
double flexiblas_time_dsyr2  [2] = {0.0,0.0};
double flexiblas_time_dtbsv  [2] = {0.0,0.0};
double flexiblas_time_dtrsm  [2] = {0.0,0.0};
double flexiblas_time_ddot   [2] = {0.0,0.0};
double flexiblas_time_dnrm2  [2] = {0.0,0.0};
double flexiblas_time_dsbmv  [2] = {0.0,0.0};
double flexiblas_time_dspr   [2] = {0.0,0.0};
double flexiblas_time_dsyr2k [2] = {0.0,0.0};
double flexiblas_time_dtpmv  [2] = {0.0,0.0};
double flexiblas_time_dtrsv  [2] = {0.0,0.0};
double flexiblas_time_dcabs1 [2] = {0.0,0.0}; 

/*-----------------------------------------------------------------------------
 *  Number of calls 
 *-----------------------------------------------------------------------------*/
unsigned long flexiblas_call_dasum  [2] = {0,0};
unsigned long flexiblas_call_dgbmv  [2] = {0,0};
unsigned long flexiblas_call_drot   [2] = {0,0};
unsigned long flexiblas_call_dscal  [2] = {0,0};
unsigned long flexiblas_call_dswap  [2] = {0,0};
unsigned long flexiblas_call_dsyr   [2] = {0,0};
unsigned long flexiblas_call_dtpsv  [2] = {0,0};
unsigned long flexiblas_call_dzasum [2] = {0,0};
unsigned long flexiblas_call_daxpy  [2] = {0,0};
unsigned long flexiblas_call_dgemm  [2] = {0,0};
unsigned long flexiblas_call_drotg  [2] = {0,0};
unsigned long flexiblas_call_dsdot  [2] = {0,0};
unsigned long flexiblas_call_dsymm  [2] = {0,0};
unsigned long flexiblas_call_dsyrk  [2] = {0,0};
unsigned long flexiblas_call_dtrmm  [2] = {0,0};
unsigned long flexiblas_call_dznrm2 [2] = {0,0};
unsigned long flexiblas_call_dgemv  [2] = {0,0};
unsigned long flexiblas_call_drotm  [2] = {0,0};
unsigned long flexiblas_call_dspmv  [2] = {0,0};
unsigned long flexiblas_call_dsymv  [2] = {0,0};
unsigned long flexiblas_call_dtbmv  [2] = {0,0};
unsigned long flexiblas_call_dtrmv  [2] = {0,0};
unsigned long flexiblas_call_dcopy  [2] = {0,0};
unsigned long flexiblas_call_dger   [2] = {0,0};
unsigned long flexiblas_call_drotmg [2] = {0,0};
unsigned long flexiblas_call_dspr2  [2] = {0,0};
unsigned long flexiblas_call_dsyr2  [2] = {0,0};
unsigned long flexiblas_call_dtbsv  [2] = {0,0};
unsigned long flexiblas_call_dtrsm  [2] = {0,0};
unsigned long flexiblas_call_ddot   [2] = {0,0};
unsigned long flexiblas_call_dnrm2  [2] = {0,0};
unsigned long flexiblas_call_dsbmv  [2] = {0,0};
unsigned long flexiblas_call_dspr   [2] = {0,0};
unsigned long flexiblas_call_dsyr2k [2] = {0,0};
unsigned long flexiblas_call_dtpmv  [2] = {0,0};
unsigned long flexiblas_call_dtrsv  [2] = {0,0};
unsigned long flexiblas_call_dcabs1 [2] = {0,0}; 

/*-----------------------------------------------------------------------------
 *  Load the Hooks for every function 
 *-----------------------------------------------------------------------------*/
int __flexiblas_hook_double(void *handle)
{
	LOAD_HOOK(dasum);
	LOAD_HOOK(daxpy);
	// LOAD_HOOK(dcabs1);
	if ( __flexiblas_current_blas.scabs1_missing == 0 ) {
		if ( LOAD_HOOK_INTERN(dcabs1) != 0 ) {
			__flexiblas_current_blas.scabs1_missing = 1; 
		}
	}

	LOAD_HOOK(dcopy);
	LOAD_HOOK(ddot);
	LOAD_HOOK(dgbmv);
	LOAD_HOOK(dgemm);
	LOAD_HOOK(dgemv);
	LOAD_HOOK(dger);
	LOAD_HOOK(dnrm2);
	LOAD_HOOK(drot);
	LOAD_HOOK(drotg);
	LOAD_HOOK(drotm);
	LOAD_HOOK(drotmg);
	LOAD_HOOK(dsbmv);
	LOAD_HOOK(dscal);
	LOAD_HOOK(dsdot);
	LOAD_HOOK(dspmv);
	LOAD_HOOK(dspr2);
	LOAD_HOOK(dspr);
	LOAD_HOOK(dswap);
	LOAD_HOOK(dsymm);
	LOAD_HOOK(dsymv);
	LOAD_HOOK(dsyr2);
	LOAD_HOOK(dsyr2k);
	LOAD_HOOK(dsyr);
	LOAD_HOOK(dsyrk);
	LOAD_HOOK(dtbmv);
	LOAD_HOOK(dtbsv);
	LOAD_HOOK(dtpmv);
	LOAD_HOOK(dtpsv);
	LOAD_HOOK(dtrmm);
	LOAD_HOOK(dtrmv);
	LOAD_HOOK(dtrsm);
	LOAD_HOOK(dtrsv);
	LOAD_HOOK(dzasum);
	LOAD_HOOK(dznrm2);
	return 0; 
}


/*-----------------------------------------------------------------------------
 *  Define the function calls 
 *-----------------------------------------------------------------------------*/
BLAS_NONVOID_FN	(double, dasum,	(Int *N, double *DX, Int *INCX),(N,DX,INCX)); 
BLAS_FN		(void, 	daxpy,	(Int *N, double *DA, double *DX, Int * INCX, double *DY, Int *INCY), (N,DA,DX,INCX,DY,INCY)); 
// BLAS_NONVOID_FN	(double,	dcabs1,	(double complex *Z),(Z));

double dcabs1_(double complex *Z){
	double ret = 0.0; 
	double te = 0, ts =  0;
	if (__flexiblas_profile) {
		ts = flexiblas_wtime(); 
	}
	if (__flexiblas_current_blas.scabs1_missing != 0 ) {
		ret= fabs(creal(*Z)) + fabs(cimag(*Z)); 
	} else {
		double (*fn)  (double complex *Z); 
		fn = flexiblas_dcabs1.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, "dcabs1_ not hooked, abort\n"); 
			abort(); 
		}
		ret = fn(Z); 
	}
	if ( __flexiblas_profile ) {
		te = flexiblas_wtime(); 
		flexiblas_call_dcabs1[0]++; 
		flexiblas_time_dcabs1[0] += (te - ts); 
	}
	return ret; 
}

double dcabs1(double complex *Z){
	return dcabs1_(Z); 
}

BLAS_FN		(void,	dcopy,	(Int *N, double *DX, Int * INCX, double *DY, Int *INCY),(N,DX,INCX, DY, INCY));
BLAS_NONVOID_FN	(double,	ddot,	(Int *N, double *DX, Int * INCX, double *DY, Int *INCY),(N,DX,INCX, DY, INCY));
BLAS_FN		(void,	dgbmv,	(char *TRANS, Int *M, Int *N, Int *KL, Int *KU, double *ALPHA, double *A, Int *LDA, double *X, Int *INCX, double *BETA, double *Y, Int *INCY), \
       		 		(TRANS,M,N,KL,KU,ALPHA,A,LDA,X, INCX, BETA, Y, INCY)) ;
BLAS_FN		(void,	dgemm,	(char * TRANSA,char *TRANSB,Int *M,Int *N,Int *K,double * ALPHA,double *A,Int * LDA,double *B,Int *LDB,double *BETA,double *C,Int *LDC),\
       		 		(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)); 
BLAS_FN		(void,	dgemv,	(char * TRANS, Int *M, Int *N, double *ALPHA, double *A, Int *LDA, double *X, Int *INCX, double *BETA, double *Y, Int*INCY), \
       		 		(TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y, INCY));  
BLAS_FN		(void,	dger,	(Int *M, Int *N, double *ALPHA, double *X, Int *INCX, double *Y, Int * INCY, double *A, Int *LDA),\
       		 		( M,  N, ALPHA, X, INCX, Y, INCY, A, LDA)); 
BLAS_NONVOID_FN	(double,	dnrm2,	(Int *N, double *X, Int *INCX), (N,X,INCX));
BLAS_FN		(void,	drot, 	(Int *N, double *DX, Int *INCX, double *DY, Int* INCY, double *C, double *S),(N,DX,INCX,DY,INCY,C,S));
BLAS_FN		(void,	drotg,	(double *DA, double *DB, double *C, double *S),(DA,DB,C,S));
BLAS_FN		(void,	drotm,	(Int *N, double *DX, Int *INCX, double *DY, Int *INCY,double *DPARAM),(N,DX,INCX,DY,INCY,DPARAM));
BLAS_FN		(void,	drotmg,	(double *DD1, double *DD2, double *DX1, double *DY1, double *DPARAM),(DD1,DD2,DX1,DY1,DPARAM));
BLAS_FN		(void,	dsbmv,	(char *UPLO, Int *N, Int* K, double *ALPHA, double *A, Int*LDA,double *X, Int *INCX, double *BETA, double *Y, Int *INCY),\
       		 		(UPLO, N,K,ALPHA,A,LDA,X,INCX,BETA, Y, INCY) );
BLAS_FN		(void,	dscal,	(Int *N, double *DA, double *DX, Int *INCX),(N,DA,DX,INCX));
BLAS_NONVOID_FN	(double,	dsdot,	(Int *N, float *SX, Int *INCX, float *SY, Int *INCY),(N,SX,INCX,SY,INCY));
BLAS_FN		(void,	dspmv,	(char *UPLO, Int *N, double *ALPHA, double *AP, double *X, Int *INCX, double *BETA, double *Y, Int *INCY),\
       		 		(UPLO, N, ALPHA, AP, X, INCX, BETA, Y, INCY));
BLAS_FN		(void,	dspr2,	(char *UPLO, Int *N, double *ALPHA, double *X, Int *INCX, double * Y, Int *INCY, double *AP),\
       		 		(UPLO, N, ALPHA, X, INCX, Y, INCY, AP));
BLAS_FN		(void,	dspr,	(char * UPLO,Int * N,double * ALPHA,double * X,Int * INCX,double *AP),(UPLO,N,ALPHA,X,INCX,AP));
BLAS_FN		(void,	dswap,	(Int *N, double * DX,Int * INCX,double *DY,Int *INCY),(N,DX,INCX,DY,INCY));
BLAS_FN		(void,	dsymm,	(char * SIDE,char * UPLO,Int *M,Int *N,double *ALPHA,double *A,Int*LDA,double*B,Int*LDB,double *BETA,double *C,Int *LDC),\
       		 		(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN		(void,	dsymv,	(char *UPLO,Int *N,double *ALPHA,double *A,Int *LDA,double *X,Int* INCX,double *BETA,double *Y,Int * INCY),\
       		 		(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY));
BLAS_FN		(void,	dsyr2,	(char *UPLO,Int *N,double *ALPHA,double *X,Int*INCX,double*Y,Int *INCY,double *A,Int *LDA),(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA));
BLAS_FN		(void,	dsyr2k,	(char *UPLO,char *TRANS,Int *N,Int *K,double * ALPHA,double * A,Int* LDA,double *B,Int *LDB,double* BETA,double *C,Int *LDC),\
       		 		(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN		(void,	dsyr,	(char *UPLO, Int *N,double *ALPHA,double*X,Int * INCX,double *A,Int *LDA),(UPLO,N,ALPHA,X,INCX,A,LDA)); 
BLAS_FN		(void,	dsyrk,	(char* UPLO, char* TRANS,Int *N,Int *K,double *ALPHA,double *A,Int *LDA,double *BETA,double *C,Int *LDC),\
       		 		(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC));
BLAS_FN		(void,	dtbmv,	(char *UPLO,char* TRANS,char*DIAG,Int *N,Int *K,double *A,Int *LDA,double *X,Int *INCX),(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX));
BLAS_FN		(void,	dtbsv,	(char *UPLO,char* TRANS,char*DIAG,Int *N,Int *K,double *A,Int* LDA,double *X,Int *INCX),(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX));
BLAS_FN		(void,	dtpmv,	(char *UPLO,char* TRANS,char*DIAG,Int* N,double *AP,double* X,Int*INCX),(UPLO,TRANS,DIAG,N,AP,X,INCX));
BLAS_FN		(void,	dtpsv,	(char *UPLO,char* TRANS,char*DIAG,Int* N,double *AP,double* X,Int*INCX),(UPLO,TRANS,DIAG,N,AP,X,INCX));
BLAS_FN		(void,	dtrmm,	(char *SIDE,char*UPLO,char *TRANSA,char*DIAG,Int *M,Int *N,double *ALPHA,double *A,Int *LDA,double *B,Int *LDB),\
       		 		(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB));
BLAS_FN		(void,	dtrmv,	(char* UPLO,char*TRANS,char *DIAG,Int *N,double *A,Int *LDA,double *X,Int *INCX),(UPLO,TRANS,DIAG,N,A,LDA,X,INCX));
BLAS_FN		(void,	dtrsm,	(char *SIDE,char*UPLO,char *TRANSA,char*DIAG,Int *M,Int *N,double *ALPHA,double *A,Int *LDA,double *B,Int *LDB),\
       		 			(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB));
BLAS_FN		(void,	dtrsv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,double *A,Int *LDA,double *X,Int *INCX),(UPLO,TRANS,DIAG,N,A,LDA,X,INCX));
BLAS_NONVOID_FN	(double,	dzasum,	(Int *N, double complex *ZX, Int *INCX),(N,ZX,INCX));
BLAS_NONVOID_FN	(double, dznrm2, (Int *N, double complex *ZX, Int *INCX),(N,ZX,INCX));


