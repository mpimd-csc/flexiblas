/* $Id$ */ 
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
#include <dlfcn.h>
#include <math.h>
#include <complex.h> 

#include "hooks.h"
/*-----------------------------------------------------------------------------
 *  Initialize the Hooks
 *-----------------------------------------------------------------------------*/
struct flexiblas_blasfn flexiblas_sasum  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_saxpy  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_scabs1 =HOOK_INIT;
struct flexiblas_blasfn flexiblas_scasum =HOOK_INIT;
struct flexiblas_blasfn flexiblas_scnrm2 =HOOK_INIT;
struct flexiblas_blasfn flexiblas_scopy  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_sdot   =HOOK_INIT;
struct flexiblas_blasfn flexiblas_sdsdot =HOOK_INIT;
struct flexiblas_blasfn flexiblas_sgbmv  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_sgemm  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_sgemv  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_sger   =HOOK_INIT;
struct flexiblas_blasfn flexiblas_snrm2  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_srot   =HOOK_INIT;
struct flexiblas_blasfn flexiblas_srotg  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_srotm  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_srotmg =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ssbmv  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_sscal  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_sspmv  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_sspr2  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_sspr   =HOOK_INIT;
struct flexiblas_blasfn flexiblas_sswap  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ssymm  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ssymv  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ssyr2  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ssyr2k =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ssyr   =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ssyrk  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_stbmv  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_stbsv  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_stpmv  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_stpsv  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_strmm  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_strmv  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_strsm  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_strsv  =HOOK_INIT;

#ifdef FLEXIBLAS_PROFILE
/*-----------------------------------------------------------------------------
 *  Profile Timing
 *-----------------------------------------------------------------------------*/
double  flexiblas_time_sasum  [2] = {0.0,0.0};
double  flexiblas_time_saxpy  [2] = {0.0,0.0};
double  flexiblas_time_scabs1 [2] = {0.0,0.0};
double  flexiblas_time_scasum [2] = {0.0,0.0};
double  flexiblas_time_scnrm2 [2] = {0.0,0.0};
double  flexiblas_time_scopy  [2] = {0.0,0.0};
double  flexiblas_time_sdot   [2] = {0.0,0.0};
double  flexiblas_time_sdsdot [2] = {0.0,0.0};
double  flexiblas_time_sgbmv  [2] = {0.0,0.0};
double  flexiblas_time_sgemm  [2] = {0.0,0.0};
double  flexiblas_time_sgemv  [2] = {0.0,0.0};
double  flexiblas_time_sger   [2] = {0.0,0.0};
double  flexiblas_time_snrm2  [2] = {0.0,0.0};
double  flexiblas_time_srot   [2] = {0.0,0.0};
double  flexiblas_time_srotg  [2] = {0.0,0.0};
double  flexiblas_time_srotm  [2] = {0.0,0.0};
double  flexiblas_time_srotmg [2] = {0.0,0.0};
double  flexiblas_time_ssbmv  [2] = {0.0,0.0};
double  flexiblas_time_sscal  [2] = {0.0,0.0};
double  flexiblas_time_sspmv  [2] = {0.0,0.0};
double  flexiblas_time_sspr2  [2] = {0.0,0.0};
double  flexiblas_time_sspr   [2] = {0.0,0.0};
double  flexiblas_time_sswap  [2] = {0.0,0.0};
double  flexiblas_time_ssymm  [2] = {0.0,0.0};
double  flexiblas_time_ssymv  [2] = {0.0,0.0};
double  flexiblas_time_ssyr2  [2] = {0.0,0.0};
double  flexiblas_time_ssyr2k [2] = {0.0,0.0};
double  flexiblas_time_ssyr   [2] = {0.0,0.0};
double  flexiblas_time_ssyrk  [2] = {0.0,0.0};
double  flexiblas_time_stbmv  [2] = {0.0,0.0};
double  flexiblas_time_stbsv  [2] = {0.0,0.0};
double  flexiblas_time_stpmv  [2] = {0.0,0.0};
double  flexiblas_time_stpsv  [2] = {0.0,0.0};
double  flexiblas_time_strmm  [2] = {0.0,0.0};
double  flexiblas_time_strmv  [2] = {0.0,0.0};
double  flexiblas_time_strsm  [2] = {0.0,0.0};
double  flexiblas_time_strsv  [2] = {0.0,0.0};

/*-----------------------------------------------------------------------------
 *  Profile Calls
 *-----------------------------------------------------------------------------*/
unsigned long  flexiblas_call_sasum  [2] = {0,0};
unsigned long  flexiblas_call_saxpy  [2] = {0,0};
unsigned long  flexiblas_call_scabs1 [2] = {0,0};
unsigned long  flexiblas_call_scasum [2] = {0,0};
unsigned long  flexiblas_call_scnrm2 [2] = {0,0};
unsigned long  flexiblas_call_scopy  [2] = {0,0};
unsigned long  flexiblas_call_sdot   [2] = {0,0};
unsigned long  flexiblas_call_sdsdot [2] = {0,0};
unsigned long  flexiblas_call_sgbmv  [2] = {0,0};
unsigned long  flexiblas_call_sgemm  [2] = {0,0};
unsigned long  flexiblas_call_sgemv  [2] = {0,0};
unsigned long  flexiblas_call_sger   [2] = {0,0};
unsigned long  flexiblas_call_snrm2  [2] = {0,0};
unsigned long  flexiblas_call_srot   [2] = {0,0};
unsigned long  flexiblas_call_srotg  [2] = {0,0};
unsigned long  flexiblas_call_srotm  [2] = {0,0};
unsigned long  flexiblas_call_srotmg [2] = {0,0};
unsigned long  flexiblas_call_ssbmv  [2] = {0,0};
unsigned long  flexiblas_call_sscal  [2] = {0,0};
unsigned long  flexiblas_call_sspmv  [2] = {0,0};
unsigned long  flexiblas_call_sspr2  [2] = {0,0};
unsigned long  flexiblas_call_sspr   [2] = {0,0};
unsigned long  flexiblas_call_sswap  [2] = {0,0};
unsigned long  flexiblas_call_ssymm  [2] = {0,0};
unsigned long  flexiblas_call_ssymv  [2] = {0,0};
unsigned long  flexiblas_call_ssyr2  [2] = {0,0};
unsigned long  flexiblas_call_ssyr2k [2] = {0,0};
unsigned long  flexiblas_call_ssyr   [2] = {0,0};
unsigned long  flexiblas_call_ssyrk  [2] = {0,0};
unsigned long  flexiblas_call_stbmv  [2] = {0,0};
unsigned long  flexiblas_call_stbsv  [2] = {0,0};
unsigned long  flexiblas_call_stpmv  [2] = {0,0};
unsigned long  flexiblas_call_stpsv  [2] = {0,0};
unsigned long  flexiblas_call_strmm  [2] = {0,0};
unsigned long  flexiblas_call_strmv  [2] = {0,0};
unsigned long  flexiblas_call_strsm  [2] = {0,0};
unsigned long  flexiblas_call_strsv  [2] = {0,0};

#endif 
/*-----------------------------------------------------------------------------
 *  Load the Hooks for every function 
 *-----------------------------------------------------------------------------*/
int __flexiblas_hook_single(void * handle){
	LOAD_HOOK(sasum);
	LOAD_HOOK(saxpy);

	/*-----------------------------------------------------------------------------
	 *  Load SCABS1, Missing in ATLAS and ACML
	 *-----------------------------------------------------------------------------*/
	if ( __flexiblas_current_blas.scabs1_missing == 0 ) {
		if ( LOAD_HOOK_INTERN(scabs1) != 0 ) {
			__flexiblas_current_blas.scabs1_missing = 1; 
		}
	}
	LOAD_HOOK(scasum);
	LOAD_HOOK(scnrm2);
	LOAD_HOOK(scopy);
	LOAD_HOOK(sdot);
	LOAD_HOOK(sdsdot);
	LOAD_HOOK(sgbmv);
	LOAD_HOOK(sgemm);
	LOAD_HOOK(sgemv);
	LOAD_HOOK(sger);
	LOAD_HOOK(snrm2);
	LOAD_HOOK(srot);
	LOAD_HOOK(srotg);
	LOAD_HOOK(srotm);
	LOAD_HOOK(srotmg);
	LOAD_HOOK(ssbmv);
	LOAD_HOOK(sscal);
	LOAD_HOOK(sspmv);
	LOAD_HOOK(sspr2);
	LOAD_HOOK(sspr);
	LOAD_HOOK(sswap);
	LOAD_HOOK(ssymm);
	LOAD_HOOK(ssymv);
	LOAD_HOOK(ssyr2);
	LOAD_HOOK(ssyr2k);
	LOAD_HOOK(ssyr);
	LOAD_HOOK(ssyrk);
	LOAD_HOOK(stbmv);
	LOAD_HOOK(stbsv);
	LOAD_HOOK(stpmv);
	LOAD_HOOK(stpsv);
	LOAD_HOOK(strmm);
	LOAD_HOOK(strmv);
	LOAD_HOOK(strsm);
	LOAD_HOOK(strsv);
	
	return 0; 
}

/*-----------------------------------------------------------------------------
 *  Define the function calls 
 *-----------------------------------------------------------------------------*/
BLAS_NONVOID_FN (float, 	sasum,	(Int *N, float *DX, Int *INCX),(N,DX,INCX)); 
BLAS_FN(void, 	saxpy,	(Int *N, float *DA, float *DX, Int * INCX, float *DY, Int *INCY), (N,DA,DX,INCX,DY,INCY)); 
// Function does not exist in ATLAS
// BLAS_FN(float,	scabs1,	(float complex *Z),(Z));
float scabs1_(float complex *Z){
	float ret = 0.0; 
#ifdef FLEXIBLAS_PROFILE 
	double te, ts = flexiblas_wtime(); 
#endif 
	if (__flexiblas_current_blas.scabs1_missing != 0 ) {
		ret= fabsf(crealf(*Z)) + fabsf(cimagf(*Z)); 
	} else {
		float (*fn)  (float complex *Z); 
		fn = flexiblas_scabs1.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, "scabs1_ not hooked, abort\n"); 
			abort(); 
		}
		ret = fn(Z); 
	}
#ifdef FLEXIBLAS_PROFILE
	te = flexiblas_wtime(); 
	flexiblas_call_scabs1[0]++; 
	flexiblas_time_scabs1[0] += (te - ts); 
#endif 
	return ret; 
}

float scabs1(float complex *Z){
	return scabs1_(Z); 
}

BLAS_NONVOID_FN(float,  sdsdot, (Int *N, float *SB, float *SX, Int *INCX, float *SY, Int *INCY), \
			(N, SB, SX, INCX, SY, INCY)); 


BLAS_NONVOID_FN(float,	scasum,	(Int *N, float complex *ZX, Int *INCX),(N,ZX,INCX));
BLAS_NONVOID_FN(float, 	scnrm2, (Int *N, float complex *ZX, Int *INCX),(N,ZX,INCX));
BLAS_FN(void,	scopy,	(Int *N, float *DX, Int * INCX, float *DY, Int *INCY),(N,DX,INCX, DY, INCY));
BLAS_NONVOID_FN(float,	sdot,	(Int *N, float *DX, Int * INCX, float *DY, Int *INCY),(N,DX,INCX, DY, INCY));
BLAS_FN(void,	sgbmv,	(char *TRANS, Int *M, Int *N, Int *KL, Int *KU, float *ALPHA, float *A, Int *LDA, float *X, Int *INCX, float *BETA, float *Y, Int *INCY), \
			(TRANS,M,N,KL,KU,ALPHA,A,LDA,X, INCX, BETA, Y, INCY)) ;
BLAS_FN(void,	sgemm,	(char * TRANSA,char *TRANSB,Int *M,Int *N,Int *K,float * ALPHA,float *A,Int * LDA,float *B,Int *LDB,float *BETA,float *C,Int *LDC),\
			(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)); 
BLAS_FN(void,	sgemv,	(char * TRANS, Int *M, Int *N, float *ALPHA, float *A, Int *LDA, float *X, Int *INCX, float *BETA, float *Y, Int*INCY), \
			(TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y, INCY));  
BLAS_FN(void,	sger,	(Int *M, Int *N, float *ALPHA, float *X, Int *INCX, float *Y, Int * INCY, float *A, Int *LDA),\
			( M,  N, ALPHA, X, INCX, Y, INCY, A, LDA)); 
BLAS_NONVOID_FN(float,	snrm2,	(Int *N, float *X, Int *INCX), (N,X,INCX));
BLAS_FN(void,	srot, 	(Int *N, float *DX, Int *INCX, float *DY, Int* INCY, float *C, float *S),(N,DX,INCX,DY,INCY,C,S));
BLAS_FN(void,	srotg,	(float *DA, float *DB, float *C, float *S),(DA,DB,C,S));
BLAS_FN(void,	srotm,	(Int *N, float *DX, Int *INCX, float *DY, Int *INCY,float *DPARAM),(N,DX,INCX,DY,INCY,DPARAM));
BLAS_FN(void,	srotmg,	(float *DD1, float *DD2, float *DX1, float *DY1, float *DPARAM),(DD1,DD2,DX1,DY1,DPARAM));
BLAS_FN(void,	ssbmv,	(char *UPLO, Int *N, Int* K, float *ALPHA, float *A, Int*LDA,float *X, Int *INCX, float *BETA, float *Y, Int *INCY),\
			(UPLO, N,K,ALPHA,A,LDA,X,INCX,BETA, Y, INCY) );
BLAS_FN(void,	sscal,	(Int *N, float *DA, float *DX, Int *INCX),(N,DA,DX,INCX));
BLAS_FN(void,	sspmv,	(char *UPLO, Int *N, float *ALPHA, float *AP, float *X, Int *INCX, float *BETA, float *Y, Int *INCY),\
			(UPLO, N, ALPHA, AP, X, INCX, BETA, Y, INCY));
BLAS_FN(void,	sspr2,	(char *UPLO, Int *N, float *ALPHA, float *X, Int *INCX, float * Y, Int *INCY, float *AP),\
			(UPLO, N, ALPHA, X, INCX, Y, INCY, AP));
BLAS_FN(void,	sspr,	(char * UPLO,Int * N,float * ALPHA,float * X,Int * INCX,float *AP),(UPLO,N,ALPHA,X,INCX,AP));
BLAS_FN(void,	sswap,	(Int *N, float * DX,Int * INCX,float *DY,Int *INCY),(N,DX,INCX,DY,INCY));
BLAS_FN(void,	ssymm,	(char * SIDE,char * UPLO,Int *M,Int *N,float *ALPHA,float *A,Int*LDA,float*B,Int*LDB,float *BETA,float *C,Int *LDC),\
			(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN(void,	ssymv,	(char *UPLO,Int *N,float *ALPHA,float *A,Int *LDA,float *X,Int* INCX,float *BETA,float *Y,Int * INCY),\
			(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY));
BLAS_FN(void,	ssyr2,	(char *UPLO,Int *N,float *ALPHA,float *X,Int*INCX,float*Y,Int *INCY,float *A,Int *LDA),(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA));
BLAS_FN(void,	ssyr2k,	(char *UPLO,char *TRANS,Int *N,Int *K,float * ALPHA,float * A,Int* LDA,float *B,Int *LDB,float* BETA,float *C,Int *LDC),\
			(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN(void,	ssyr,	(char *UPLO, Int *N,float *ALPHA,float*X,Int * INCX,float *A,Int *LDA),(UPLO,N,ALPHA,X,INCX,A,LDA)); 
BLAS_FN(void,	ssyrk,	(char* UPLO, char* TRANS,Int *N,Int *K,float *ALPHA,float *A,Int *LDA,float *BETA,float *C,Int *LDC),\
			(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC));
BLAS_FN(void,	stbmv,	(char *UPLO,char* TRANS,char*DIAG,Int *N,Int *K,float *A,Int *LDA,float *X,Int *INCX),(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX));
BLAS_FN(void,	stbsv,	(char *UPLO,char* TRANS,char*DIAG,Int *N,Int *K,float *A,Int* LDA,float *X,Int *INCX),(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX));
BLAS_FN(void,	stpmv,	(char *UPLO,char* TRANS,char*DIAG,Int* N,float *AP,float* X,Int*INCX),(UPLO,TRANS,DIAG,N,AP,X,INCX));
BLAS_FN(void,	stpsv,	(char *UPLO,char* TRANS,char*DIAG,Int* N,float *AP,float* X,Int*INCX),(UPLO,TRANS,DIAG,N,AP,X,INCX));
BLAS_FN(void,	strmm,	(char *SIDE,char*UPLO,char *TRANSA,char*DIAG,Int *M,Int *N,float *ALPHA,float *A,Int *LDA,float *B,Int *LDB),\
			(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB));
BLAS_FN(void,	strmv,	(char* UPLO,char*TRANS,char *DIAG,Int *N,float *A,Int *LDA,float *X,Int *INCX),(UPLO,TRANS,DIAG,N,A,LDA,X,INCX));
BLAS_FN(void,	strsm,	(char *SIDE,char*UPLO,char *TRANSA,char*DIAG,Int *M,Int *N,float *ALPHA,float *A,Int *LDA,float *B,Int *LDB),\
				(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB));
BLAS_FN(void,	strsv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,float *A,Int *LDA,float *X,Int *INCX),(UPLO,TRANS,DIAG,N,A,LDA,X,INCX));

