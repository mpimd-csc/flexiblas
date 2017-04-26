/* $Id: complex16.c 3758 2013-10-10 14:35:20Z komart $ */ 
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
#include "hooks.h"


/*-----------------------------------------------------------------------------
 *  Initialize Hooks
 *-----------------------------------------------------------------------------*/
struct flexiblas_blasfn flexiblas_zaxpy =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zcopy =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zdotc =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zdotu =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zdrot =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zdscal=HOOK_INIT;
struct flexiblas_blasfn flexiblas_zgbmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zgemm =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zgemv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zgerc =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zgeru =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zhbmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zhemm =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zhemv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zher2 =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zher2k=HOOK_INIT;
struct flexiblas_blasfn flexiblas_zher  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zherk =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zhpmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zhpr2 =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zhpr  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zrotg =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zscal =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zswap =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zsymm =HOOK_INIT;
struct flexiblas_blasfn flexiblas_zsyr2k=HOOK_INIT;
struct flexiblas_blasfn flexiblas_zsyrk =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ztbmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ztbsv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ztpmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ztpsv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ztrmm =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ztrmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ztrsm =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ztrsv =HOOK_INIT;

/*-----------------------------------------------------------------------------
 *  Profile timing
 *-----------------------------------------------------------------------------*/
double  flexiblas_time_zaxpy [2] = {0.0,0.0};
double  flexiblas_time_zcopy [2] = {0.0,0.0};
double  flexiblas_time_zdotc [2] = {0.0,0.0};
double  flexiblas_time_zdotu [2] = {0.0,0.0};
double  flexiblas_time_zdrot [2] = {0.0,0.0};
double  flexiblas_time_zdscal[2] = {0.0,0.0};
double  flexiblas_time_zgbmv [2] = {0.0,0.0};
double  flexiblas_time_zgemm [2] = {0.0,0.0};
double  flexiblas_time_zgemv [2] = {0.0,0.0};
double  flexiblas_time_zgerc [2] = {0.0,0.0};
double  flexiblas_time_zgeru [2] = {0.0,0.0};
double  flexiblas_time_zhbmv [2] = {0.0,0.0};
double  flexiblas_time_zhemm [2] = {0.0,0.0};
double  flexiblas_time_zhemv [2] = {0.0,0.0};
double  flexiblas_time_zher2 [2] = {0.0,0.0};
double  flexiblas_time_zher2k[2] = {0.0,0.0};
double  flexiblas_time_zher  [2] = {0.0,0.0};
double  flexiblas_time_zherk [2] = {0.0,0.0};
double  flexiblas_time_zhpmv [2] = {0.0,0.0};
double  flexiblas_time_zhpr2 [2] = {0.0,0.0};
double  flexiblas_time_zhpr  [2] = {0.0,0.0};
double  flexiblas_time_zrotg [2] = {0.0,0.0};
double  flexiblas_time_zscal [2] = {0.0,0.0};
double  flexiblas_time_zswap [2] = {0.0,0.0};
double  flexiblas_time_zsymm [2] = {0.0,0.0};
double  flexiblas_time_zsyr2k[2] = {0.0,0.0};
double  flexiblas_time_zsyrk [2] = {0.0,0.0};
double  flexiblas_time_ztbmv [2] = {0.0,0.0};
double  flexiblas_time_ztbsv [2] = {0.0,0.0};
double  flexiblas_time_ztpmv [2] = {0.0,0.0};
double  flexiblas_time_ztpsv [2] = {0.0,0.0};
double  flexiblas_time_ztrmm [2] = {0.0,0.0};
double  flexiblas_time_ztrmv [2] = {0.0,0.0};
double  flexiblas_time_ztrsm [2] = {0.0,0.0};
double  flexiblas_time_ztrsv [2] = {0.0,0.0};

/*-----------------------------------------------------------------------------
 *  Profile number of calls
 *-----------------------------------------------------------------------------*/
unsigned long flexiblas_call_zaxpy [2] = {0,0};
unsigned long flexiblas_call_zcopy [2] = {0,0};
unsigned long flexiblas_call_zdotc [2] = {0,0};
unsigned long flexiblas_call_zdotu [2] = {0,0};
unsigned long flexiblas_call_zdrot [2] = {0,0};
unsigned long flexiblas_call_zdscal[2] = {0,0};
unsigned long flexiblas_call_zgbmv [2] = {0,0};
unsigned long flexiblas_call_zgemm [2] = {0,0};
unsigned long flexiblas_call_zgemv [2] = {0,0};
unsigned long flexiblas_call_zgerc [2] = {0,0};
unsigned long flexiblas_call_zgeru [2] = {0,0};
unsigned long flexiblas_call_zhbmv [2] = {0,0};
unsigned long flexiblas_call_zhemm [2] = {0,0};
unsigned long flexiblas_call_zhemv [2] = {0,0};
unsigned long flexiblas_call_zher2 [2] = {0,0};
unsigned long flexiblas_call_zher2k[2] = {0,0};
unsigned long flexiblas_call_zher  [2] = {0,0};
unsigned long flexiblas_call_zherk [2] = {0,0};
unsigned long flexiblas_call_zhpmv [2] = {0,0};
unsigned long flexiblas_call_zhpr2 [2] = {0,0};
unsigned long flexiblas_call_zhpr  [2] = {0,0};
unsigned long flexiblas_call_zrotg [2] = {0,0};
unsigned long flexiblas_call_zscal [2] = {0,0};
unsigned long flexiblas_call_zswap [2] = {0,0};
unsigned long flexiblas_call_zsymm [2] = {0,0};
unsigned long flexiblas_call_zsyr2k[2] = {0,0};
unsigned long flexiblas_call_zsyrk [2] = {0,0};
unsigned long flexiblas_call_ztbmv [2] = {0,0};
unsigned long flexiblas_call_ztbsv [2] = {0,0};
unsigned long flexiblas_call_ztpmv [2] = {0,0};
unsigned long flexiblas_call_ztpsv [2] = {0,0};
unsigned long flexiblas_call_ztrmm [2] = {0,0};
unsigned long flexiblas_call_ztrmv [2] = {0,0};
unsigned long flexiblas_call_ztrsm [2] = {0,0};
unsigned long flexiblas_call_ztrsv [2] = {0,0};


/*-----------------------------------------------------------------------------
 *  Load the Hooks for every function 
 *-----------------------------------------------------------------------------*/
int __flexiblas_hook_complex16(void * handle){
	LOAD_HOOK(zaxpy);
	LOAD_HOOK(zcopy);
	LOAD_HOOK2(zdotc,cblas_zdotc_sub);
	LOAD_HOOK2(zdotu,cblas_zdotu_sub);
	LOAD_HOOK(zdrot);
	LOAD_HOOK(zdscal);
	LOAD_HOOK(zgbmv);
	LOAD_HOOK(zgemm);
	LOAD_HOOK(zgemv);
	LOAD_HOOK(zgerc);
	LOAD_HOOK(zgeru);
	LOAD_HOOK(zhbmv);
	LOAD_HOOK(zhemm);
	LOAD_HOOK(zhemv);
	LOAD_HOOK(zher2);
	LOAD_HOOK(zher2k);
	LOAD_HOOK(zher);
	LOAD_HOOK(zherk);
	LOAD_HOOK(zhpmv);
	LOAD_HOOK(zhpr2);
	LOAD_HOOK(zhpr);
	LOAD_HOOK(zrotg);
	LOAD_HOOK(zscal);
	LOAD_HOOK(zswap);
	LOAD_HOOK(zsymm);
	LOAD_HOOK(zsyr2k);
	LOAD_HOOK(zsyrk);
	LOAD_HOOK(ztbmv);
	LOAD_HOOK(ztbsv);
	LOAD_HOOK(ztpmv);
	LOAD_HOOK(ztpsv);
	LOAD_HOOK(ztrmm);
	LOAD_HOOK(ztrmv);
	LOAD_HOOK(ztrsm);
	LOAD_HOOK(ztrsv);
	return 0; 
}

/*-----------------------------------------------------------------------------
 *  Define the function calls 
 *-----------------------------------------------------------------------------*/

#ifdef Complex 
#undef Complex
#endif 
#define Complex double complex
BLAS_FN(void,	zaxpy,	(Int *N, Complex *CA, Complex *CX,Int *INCX,Complex *CY,Int * INCY),\
			(N,CA,CX,INCX,CY,INCY)); 
BLAS_FN(void,	zcopy,	(Int *N, Complex *CX,Int *INCX,Complex *CY,Int *INCY),\
			(N,CX,INCX,CY,INCY)); 
BLAS_FN(void,	zgbmv,	(char *TRANS,Int *M,Int *N,Int *KL,Int *KU,Complex *ALPHA,Complex *A,Int*LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY),\
			(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)); 
BLAS_FN(void,	zgemm,	(char *TRANSA,char*TRANSB,Int *M,Int *N,Int *K,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int*LDB,Complex *BETA,Complex *C, Int*LDC),\
			(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)); 
BLAS_FN(void,	zgemv,	(char * TRANS,Int *M,Int*N,Complex *ALPHA,Complex *A,Int *LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY),\
			(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)); 
BLAS_FN(void,	zgerc,	(Int *M, Int*N,Complex *ALPHA,Complex*X,Int*INCX,Complex *Y,Int *INCY,Complex *A,Int *LDA),\
			(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)); 
BLAS_FN(void,	zgeru,	(Int *M,Int *N,Complex *ALPHA,Complex *X, Int *INCX,Complex *Y,Int *INCY,Complex* A,Int *LDA),\
			(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)); 
BLAS_FN(void,	zhbmv,	(char* UPLO,Int *N,Int*K,Complex *ALPHA,Complex*A,Int*LDA,Complex * X,Int * INCX,Complex *BETA,Complex *Y,Int* INCY),\
			(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)); 
BLAS_FN(void,	zhemm,	(char* SIDE,char*UPLO,Int *M,Int*N,Complex*ALPHA,Complex*A,Int *LDA,Complex *B,Int *LDB,Complex * BETA,Complex *C,Int *LDC),\
			(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN(void,	zhemv,	(char*UPLO,Int*N,Complex *ALPHA,Complex*A,Int*LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY),\
			(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)); 
BLAS_FN(void,	zher2,	(char *UPLO,Int *N,Complex * ALPHA,Complex *X,Int *INCX,Complex *Y,Int *INCY,Complex *A,Int *LDA),\
			(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)); 
BLAS_FN(void,	zher2k,	(char *UPLO,char *TRANS,Int *N,Int*K,Complex*ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int*LDC),\
			(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN(void,	zher,	(char *UPLO,Int *N,Complex *ALPHA,Complex *X,Int *INCX,Complex *A,Int *LDA),\
			(UPLO,N,ALPHA,X,INCX,A,LDA));
BLAS_FN(void,	zherk,	(char *UPLO,char *TRANS,Int *N,Int*K,Complex *ALPHA,Complex *A,Int *LDA,Complex *BETA,Complex*C,Int *LDC),\
			(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC));
BLAS_FN(void,	zhpmv,	(char *UPLO,Int *N,Complex *ALPHA,Complex *AP,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY),\
			(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY));
BLAS_FN(void,	zhpr2,	(char *UPLO,Int *N, Complex *ALPHA,Complex *X,Int *INCX,Complex *Y,Int *INCY,Complex *AP),\
			(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)); 
BLAS_FN(void,	zhpr,	(char *UPLO,Int *N,Complex *ALPHA,Complex *X,Int*INCX,Complex *AP),\
			(UPLO,N,ALPHA,X,INCX,AP)); 
BLAS_FN(void,	zrotg,	(Complex *CA,Complex *CB,double *C,Complex *S),\
			(CA,CB,C,S)); 
BLAS_FN(void,	zscal,	(Int *N,Complex *CA,Complex *CX,Int *INCX),\
			(N,CA,CX,INCX));
BLAS_FN(void,	zdrot,	(Int * N, Complex *CX, Int * INCX, Complex *CY, Int * INCY, double *C, double *S ),\
			( N, CX, INCX, CY, INCY, C, S ));
BLAS_FN(void,	zdscal,	(Int *N,double *SA,Complex *CX,Int * INCX),\
			(N,SA,CX,INCX));
BLAS_FN(void,	zswap,	(Int *N,Complex *CX,Int *INCX,Complex *CY,Int *INCY),\
			(N,CX,INCX,CY,INCY));
BLAS_FN(void,	zsymm,	(char *SIDE,char *UPLO,Int *M,Int*N,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int *LDC),\
			(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN(void,	zsyr2k,	(char *UPLO,char *TRANS,Int *N,Int *K,Complex *ALPHA,Complex *A,Int* LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int *LDC),\
			(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN(void,	zsyrk,	(char *UPLO,char *TRANS,Int *N,Int *K,Complex *ALPHA,Complex *A,Int *LDA,Complex *BETA,Complex *C,Int *LDC),\
			(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC));
BLAS_FN(void,	ztbmv,	(char *UPLO,char *TRANS,char*DIAG,Int *N,Int *K,Complex *A,Int *LDA,Complex *X,Int *INCX),\
			(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX));
BLAS_FN(void,	ztbsv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,Int*K,Complex *A,Int *LDA,Complex *X,Int *INCX),\
			(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX));
BLAS_FN(void,	ztpmv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,Complex *AP,Complex *X,Int *INCX),\
			(UPLO,TRANS,DIAG,N,AP,X,INCX));
BLAS_FN(void,	ztpsv,	(char * UPLO,char *TRANS,char *DIAG,Int *N, Complex *AP,Complex *X,Int *INCX),\
			(UPLO,TRANS,DIAG,N,AP,X,INCX));
BLAS_FN(void,	ztrmm,	(char *SIDE,char *UPLO,char*TRANSA,char* DIAG,Int *M,Int *N,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB),\
			(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB));
BLAS_FN(void,	ztrmv,	(char *UPLO,char*TRANS,char* DIAG,Int * N,Complex *A,Int *LDA,Complex *X,Int *INCX),\
			(UPLO,TRANS,DIAG,N,A,LDA,X,INCX));
BLAS_FN(void,	ztrsm,	(char *SIDE,char*UPLO,char*TRANSA,char*DIAG,Int*M,Int*N,Complex * ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB),\
			(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB));
BLAS_FN(void,	ztrsv,	(char *UPLO,char*TRANS,char *DIAG,Int *N,Complex *A,Int *LDA,Complex *X,Int *INCX),\
			(UPLO,TRANS,DIAG,N,A,LDA,X,INCX));
/*
 BLAS_FN(double complex,	zdotc,	(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY), (N,CX,INCX,CY,INCY));
 BLAS_FN(double complex,	zdotu,	(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY), (N,CX,INCX,CY,INCY));
 */
#ifdef USE_INTERFACE_INTEL 
void zdotc_(Complex *rp, Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	Complex ret = 0; 
	double ts = 0;
	if (__flexiblas_profile ) {
		ts = flexiblas_wtime(); 
	}
	
	if (__flexiblas_current_blas.zdotc_is_intel != 0 ) {
		void (*fn) (Complex *ret, Int *N, Complex *CX, Int *INCX, Complex *CY, Int *INCY); 
		fn = flexiblas_zdotc.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "zdotc_ not hooked, abort\n"); 
			abort(); 
		}
		fn(&ret, N, CX, INCX, CY, INCY); 
	} else {
		Complex (*fn)  (Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY); 
		fn = flexiblas_zdotc.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "zdotc_ not hooked, abort\n"); 
			abort(); 
		}
		ret = fn(N, CX, INCX, CY, INCY); 
	}
	if ( __flexiblas_profile) {
		flexiblas_time_zdotc[0] = flexiblas_wtime() - ts; 
		flexiblas_call_zdotc[0] ++; 
	}
	*rp = ret; 
	return; 
	
}
void zdotc(Complex *rp, Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	zdotc_(rp,N,CX,INCX,CY,INCY); 
}


void zdotu_(Complex *rp, Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	Complex ret = 0;
	double ts = 0;
	if ( __flexiblas_profile ){
		ts = flexiblas_wtime(); 
	}
	if (__flexiblas_current_blas.zdotu_is_intel != 0 ) {
		void (*fn) (Complex *ret, Int *N, Complex *CX, Int *INCX, Complex *CY, Int *INCY); 
		fn = flexiblas_zdotu.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX  "zdotc_ not hooked, abort\n"); 
			abort(); 
		}
		fn(&ret, N, CX, INCX, CY, INCY); 
	} else {
		Complex (*fn)  (Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY); 
		fn = flexiblas_zdotu.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "zdotu_ not hooked, abort\n"); 
			abort(); 
		}
		ret = fn(N, CX, INCX, CY, INCY); 
	}
	if ( __flexiblas_profile) {
		flexiblas_time_zdotu[0] = flexiblas_wtime() - ts; 
		flexiblas_call_zdotu[0] ++; 
	}
	*rp = ret; 
	return; 
}
void zdotu(Complex *rp,Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	zdotu_(rp,N,CX,INCX,CY,INCY); 
}

#else
// The Intel MKL library can have a different calling sequence 
double complex zdotc_(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	Complex ret = 0; 
	double ts = 0;
	if (__flexiblas_profile ) {
		ts = flexiblas_wtime(); 
	}
	
	if (__flexiblas_current_blas.zdotc_is_intel != 0 ) {
		void (*fn) (Complex *ret, Int *N, Complex *CX, Int *INCX, Complex *CY, Int *INCY); 
		fn = flexiblas_zdotc.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "zdotc_ not hooked, abort\n"); 
			abort(); 
		}
		fn(&ret, N, CX, INCX, CY, INCY); 
	} else {
		Complex (*fn)  (Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY); 
		fn = flexiblas_zdotc.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "zdotc_ not hooked, abort\n"); 
			abort(); 
		}
		ret = fn(N, CX, INCX, CY, INCY); 
	}
	if ( __flexiblas_profile) {
		flexiblas_time_zdotc[0] = flexiblas_wtime() - ts; 
		flexiblas_call_zdotc[0] ++; 
	}
	return ret; 
	
}
double complex zdotc(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	return zdotc_(N,CX,INCX,CY,INCY); 
}


double complex zdotu_(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	Complex ret = 0;
	double ts = 0;
	if ( __flexiblas_profile ){
		ts = flexiblas_wtime(); 
	}
	if (__flexiblas_current_blas.zdotu_is_intel != 0 ) {
		void (*fn) (Complex *ret, Int *N, Complex *CX, Int *INCX, Complex *CY, Int *INCY); 
		fn = flexiblas_zdotu.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX  "zdotc_ not hooked, abort\n"); 
			abort(); 
		}
		fn(&ret, N, CX, INCX, CY, INCY); 
	} else {
		Complex (*fn)  (Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY); 
		fn = flexiblas_zdotu.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "zdotu_ not hooked, abort\n"); 
			abort(); 
		}
		ret = fn(N, CX, INCX, CY, INCY); 
	}
	if ( __flexiblas_profile) {
		flexiblas_time_zdotu[0] = flexiblas_wtime() - ts; 
		flexiblas_call_zdotu[0] ++; 
	}
	return ret; 
}
double complex zdotu(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	return zdotu_(N,CX,INCX,CY,INCY); 
}

#endif
