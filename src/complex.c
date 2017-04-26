/* $Id: complex.c 3758 2013-10-10 14:35:20Z komart $ */ 
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


#ifdef EXTBLAS
#include "extblas.h"
#endif 

/*-----------------------------------------------------------------------------
 *  Initialize the Hooks
 *-----------------------------------------------------------------------------*/
struct flexiblas_blasfn flexiblas_caxpy =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ccopy =HOOK_INIT;
struct flexiblas_blasfn flexiblas_cdotc =HOOK_INIT;
struct flexiblas_blasfn flexiblas_cdotu =HOOK_INIT;
struct flexiblas_blasfn flexiblas_cgbmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_cgemm =HOOK_INIT;
struct flexiblas_blasfn flexiblas_cgemv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_cgerc =HOOK_INIT;
struct flexiblas_blasfn flexiblas_cgeru =HOOK_INIT;
struct flexiblas_blasfn flexiblas_chbmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_chemm =HOOK_INIT;
struct flexiblas_blasfn flexiblas_chemv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_cher2 =HOOK_INIT;
struct flexiblas_blasfn flexiblas_cher2k=HOOK_INIT;
struct flexiblas_blasfn flexiblas_cher  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_cherk =HOOK_INIT;
struct flexiblas_blasfn flexiblas_chpmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_chpr2 =HOOK_INIT;
struct flexiblas_blasfn flexiblas_chpr  =HOOK_INIT;
struct flexiblas_blasfn flexiblas_crotg =HOOK_INIT;
struct flexiblas_blasfn flexiblas_cscal =HOOK_INIT;
struct flexiblas_blasfn flexiblas_csrot =HOOK_INIT;
struct flexiblas_blasfn flexiblas_csscal=HOOK_INIT;
struct flexiblas_blasfn flexiblas_cswap =HOOK_INIT;
struct flexiblas_blasfn flexiblas_csymm =HOOK_INIT;
struct flexiblas_blasfn flexiblas_csyr2k=HOOK_INIT;
struct flexiblas_blasfn flexiblas_csyrk =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ctbmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ctbsv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ctpmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ctpsv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ctrmm =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ctrmv =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ctrsm =HOOK_INIT;
struct flexiblas_blasfn flexiblas_ctrsv =HOOK_INIT;

#ifdef EXTBLAS 
struct flexiblas_blasfn flexiblas_caxpby = HOOK_INIT;
struct flexiblas_blasfn flexiblas_comatcopy = HOOK_INIT; 
struct flexiblas_blasfn flexiblas_cimatcopy = HOOK_INIT; 
#endif
/*-----------------------------------------------------------------------------
 *  Profile timer
 *-----------------------------------------------------------------------------*/
double flexiblas_time_caxpy [2] = {0.0,0.0};
double flexiblas_time_ccopy [2] = {0.0,0.0};
double flexiblas_time_cdotc [2] = {0.0,0.0};
double flexiblas_time_cdotu [2] = {0.0,0.0};
double flexiblas_time_cgbmv [2] = {0.0,0.0};
double flexiblas_time_cgemm [2] = {0.0,0.0};
double flexiblas_time_cgemv [2] = {0.0,0.0};
double flexiblas_time_cgerc [2] = {0.0,0.0};
double flexiblas_time_cgeru [2] = {0.0,0.0};
double flexiblas_time_chbmv [2] = {0.0,0.0};
double flexiblas_time_chemm [2] = {0.0,0.0};
double flexiblas_time_chemv [2] = {0.0,0.0};
double flexiblas_time_cher2 [2] = {0.0,0.0};
double flexiblas_time_cher2k[2] = {0.0,0.0};
double flexiblas_time_cher  [2] = {0.0,0.0};
double flexiblas_time_cherk [2] = {0.0,0.0};
double flexiblas_time_chpmv [2] = {0.0,0.0};
double flexiblas_time_chpr2 [2] = {0.0,0.0};
double flexiblas_time_chpr  [2] = {0.0,0.0};
double flexiblas_time_crotg [2] = {0.0,0.0};
double flexiblas_time_cscal [2] = {0.0,0.0};
double flexiblas_time_csrot [2] = {0.0,0.0};
double flexiblas_time_csscal[2] = {0.0,0.0};
double flexiblas_time_cswap [2] = {0.0,0.0};
double flexiblas_time_csymm [2] = {0.0,0.0};
double flexiblas_time_csyr2k[2] = {0.0,0.0};
double flexiblas_time_csyrk [2] = {0.0,0.0};
double flexiblas_time_ctbmv [2] = {0.0,0.0};
double flexiblas_time_ctbsv [2] = {0.0,0.0};
double flexiblas_time_ctpmv [2] = {0.0,0.0};
double flexiblas_time_ctpsv [2] = {0.0,0.0};
double flexiblas_time_ctrmm [2] = {0.0,0.0};
double flexiblas_time_ctrmv [2] = {0.0,0.0};
double flexiblas_time_ctrsm [2] = {0.0,0.0};
double flexiblas_time_ctrsv [2] = {0.0,0.0};
#ifdef EXTBLAS 
double flexiblas_time_caxpby [2] = {0.0,0.0};
double flexiblas_time_comatcopy [2] = {0.0 ,0.0}; 
double flexiblas_time_cimatcopy [2] = {0.0 ,0.0}; 
#endif

/*-----------------------------------------------------------------------------
 *  Profile call counter
 *-----------------------------------------------------------------------------*/
unsigned long  flexiblas_call_caxpy [2] = {0,0};
unsigned long  flexiblas_call_ccopy [2] = {0,0};
unsigned long  flexiblas_call_cdotc [2] = {0,0};
unsigned long  flexiblas_call_cdotu [2] = {0,0};
unsigned long  flexiblas_call_cgbmv [2] = {0,0};
unsigned long  flexiblas_call_cgemm [2] = {0,0};
unsigned long  flexiblas_call_cgemv [2] = {0,0};
unsigned long  flexiblas_call_cgerc [2] = {0,0};
unsigned long  flexiblas_call_cgeru [2] = {0,0};
unsigned long  flexiblas_call_chbmv [2] = {0,0};
unsigned long  flexiblas_call_chemm [2] = {0,0};
unsigned long  flexiblas_call_chemv [2] = {0,0};
unsigned long  flexiblas_call_cher2 [2] = {0,0};
unsigned long  flexiblas_call_cher2k[2] = {0,0};
unsigned long  flexiblas_call_cher  [2] = {0,0};
unsigned long  flexiblas_call_cherk [2] = {0,0};
unsigned long  flexiblas_call_chpmv [2] = {0,0};
unsigned long  flexiblas_call_chpr2 [2] = {0,0};
unsigned long  flexiblas_call_chpr  [2] = {0,0};
unsigned long  flexiblas_call_crotg [2] = {0,0};
unsigned long  flexiblas_call_cscal [2] = {0,0};
unsigned long  flexiblas_call_csrot [2] = {0,0};
unsigned long  flexiblas_call_csscal[2] = {0,0};
unsigned long  flexiblas_call_cswap [2] = {0,0};
unsigned long  flexiblas_call_csymm [2] = {0,0};
unsigned long  flexiblas_call_csyr2k[2] = {0,0};
unsigned long  flexiblas_call_csyrk [2] = {0,0};
unsigned long  flexiblas_call_ctbmv [2] = {0,0};
unsigned long  flexiblas_call_ctbsv [2] = {0,0};
unsigned long  flexiblas_call_ctpmv [2] = {0,0};
unsigned long  flexiblas_call_ctpsv [2] = {0,0};
unsigned long  flexiblas_call_ctrmm [2] = {0,0};
unsigned long  flexiblas_call_ctrmv [2] = {0,0};
unsigned long  flexiblas_call_ctrsm [2] = {0,0};
unsigned long  flexiblas_call_ctrsv [2] = {0,0};
#ifdef EXTBLAS 
unsigned long  flexiblas_call_caxpby [2] = {0,0};
unsigned long  flexiblas_call_comatcopy [2] = { 0,0}; 
unsigned long  flexiblas_call_cimatcopy [2] = { 0,0}; 
#endif


/*-----------------------------------------------------------------------------
 *  Load the Hooks for every function 
 *-----------------------------------------------------------------------------*/
int __flexiblas_hook_complex(void * handle){
	LOAD_HOOK(caxpy);
	LOAD_HOOK(ccopy);
	LOAD_HOOK2(cdotc,cblas_cdotc_sub);
	LOAD_HOOK2(cdotu,cblas_cdotu_sub);
	LOAD_HOOK(cgbmv);
	LOAD_HOOK(cgemm);
	LOAD_HOOK(cgemv);
	LOAD_HOOK(cgerc);
	LOAD_HOOK(cgeru);
	LOAD_HOOK(chbmv);
	LOAD_HOOK(chemm);
	LOAD_HOOK(chemv);
	LOAD_HOOK(cher2);
	LOAD_HOOK(cher2k);
	LOAD_HOOK(cher);
	LOAD_HOOK(cherk);
	LOAD_HOOK(chpmv);
	LOAD_HOOK(chpr2);
	LOAD_HOOK(chpr);
	LOAD_HOOK(crotg);
	LOAD_HOOK(cscal);
	LOAD_HOOK(csrot);
	LOAD_HOOK(csscal);
	LOAD_HOOK(cswap);
	LOAD_HOOK(csymm);
	LOAD_HOOK(csyr2k);
	LOAD_HOOK(csyrk);
	LOAD_HOOK(ctbmv);
	LOAD_HOOK(ctbsv);
	LOAD_HOOK(ctpmv);
	LOAD_HOOK(ctpsv);
	LOAD_HOOK(ctrmm);
	LOAD_HOOK(ctrmv);
	LOAD_HOOK(ctrsm);
	LOAD_HOOK(ctrsv);

#ifdef EXTBLAS 
	/* Load AXPBY  */
	if ( LOAD_HOOK_INTERN(caxpby) != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "Flexiblas CAXPBY loaded.\n");
		}
		flexiblas_caxpby.call_fblas = fcaxpby_; 
		flexiblas_caxpby.call_cblas = NULL; 
	}

	/* Load OMATCOPY */
	if ( LOAD_HOOK_INTERN(comatcopy) != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "Flexiblas COMATCOPY loaded.\n");
		}
		flexiblas_comatcopy.call_fblas = fcomatcopy_; 
		flexiblas_comatcopy.call_cblas = NULL; 
	}

	/* Load IMATCOPY */
	if ( LOAD_HOOK_INTERN(cimatcopy) != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "Flexiblas CIMATCOPY loaded.\n");
		}
		flexiblas_cimatcopy.call_fblas = fcimatcopy_; 
		flexiblas_cimatcopy.call_cblas = NULL; 
	}

#endif 

	return 0; 
}

/*-----------------------------------------------------------------------------
 *  Define the function calls 
 *-----------------------------------------------------------------------------*/
#ifdef Complex 
#undef Complex
#endif 

#define Complex float complex
BLAS_FN		(void,	caxpy,	(Int *N, Complex *CA, Complex *CX,Int *INCX,Complex *CY,Int * INCY),\
       		 		(N,CA,CX,INCX,CY,INCY)); 
BLAS_FN		(void,	ccopy,	(Int *N, Complex *CX,Int *INCX,Complex *CY,Int *INCY),\
       		 		(N,CX,INCX,CY,INCY)); 
BLAS_FN		(void,	cgbmv,	(char *TRANS,Int *M,Int *N,Int *KL,Int *KU,Complex *ALPHA,Complex *A,Int*LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY),\
       		 		(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)); 
BLAS_FN		(void,	cgemm,	(char *TRANSA,char*TRANSB,Int *M,Int *N,Int *K,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int*LDB,Complex *BETA,Complex *C, Int*LDC),\
       		 		(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)); 
BLAS_FN		(void,	cgemv,	(char * TRANS,Int *M,Int*N,Complex *ALPHA,Complex *A,Int *LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY),\
       		 		(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)); 
BLAS_FN		(void,	cgerc,	(Int *M, Int*N,Complex *ALPHA,Complex*X,Int*INCX,Complex *Y,Int *INCY,Complex *A,Int *LDA),\
       		 		(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)); 
BLAS_FN		(void,	cgeru,	(Int *M,Int *N,Complex *ALPHA,Complex *X, Int *INCX,Complex *Y,Int *INCY,Complex* A,Int *LDA),\
       		 		(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)); 
BLAS_FN		(void,	chbmv,	(char* UPLO,Int *N,Int*K,Complex *ALPHA,Complex*A,Int*LDA,Complex * X,Int * INCX,Complex *BETA,Complex *Y,Int* INCY),\
       		 		(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)); 
BLAS_FN		(void,	chemm,	(char* SIDE,char*UPLO,Int *M,Int*N,Complex*ALPHA,Complex*A,Int *LDA,Complex *B,Int *LDB,Complex * BETA,Complex *C,Int *LDC),\
       		 		(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN		(void,	chemv,	(char*UPLO,Int*N,Complex *ALPHA,Complex*A,Int*LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY),\
       		 		(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)); 
BLAS_FN		(void,	cher2,	(char *UPLO,Int *N,Complex * ALPHA,Complex *X,Int *INCX,Complex *Y,Int *INCY,Complex *A,Int *LDA),\
       		 		(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)); 
BLAS_FN		(void,	cher2k,	(char *UPLO,char *TRANS,Int *N,Int*K,Complex*ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int*LDC),\
       		 		(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN		(void,	cher,	(char *UPLO,Int *N,Complex *ALPHA,Complex *X,Int *INCX,Complex *A,Int *LDA),\
       		 		(UPLO,N,ALPHA,X,INCX,A,LDA));
BLAS_FN		(void,	cherk,	(char *UPLO,char *TRANS,Int *N,Int*K,Complex *ALPHA,Complex *A,Int *LDA,Complex *BETA,Complex*C,Int *LDC),\
       		 		(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC));
BLAS_FN		(void,	chpmv,	(char *UPLO,Int *N,Complex *ALPHA,Complex *AP,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY),\
       		 		(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY));
BLAS_FN		(void,	chpr2,	(char *UPLO,Int *N, Complex *ALPHA,Complex *X,Int *INCX,Complex *Y,Int *INCY,Complex *AP),\
       		 		(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)); 
BLAS_FN		(void,	chpr,	(char *UPLO,Int *N,Complex *ALPHA,Complex *X,Int*INCX,Complex *AP),\
       		 		(UPLO,N,ALPHA,X,INCX,AP)); 
BLAS_FN		(void,	crotg,	(Complex *CA,Complex *CB,float *C,Complex *S),\
       		 		(CA,CB,C,S)); 
BLAS_FN		(void,	cscal,	(Int *N,Complex *CA,Complex *CX,Int *INCX),\
       		 		(N,CA,CX,INCX));
BLAS_FN		(void,	csrot,	(Int * N, Complex *CX, Int * INCX, Complex *CY, Int * INCY, float *C, float *S ),\
       		 		( N, CX, INCX, CY, INCY, C, S ));
BLAS_FN		(void,	csscal,	(Int *N,float *SA,Complex *CX,Int * INCX),\
       		 		(N,SA,CX,INCX));
BLAS_FN		(void,	cswap,	(Int *N,Complex *CX,Int *INCX,Complex *CY,Int *INCY),\
       		 		(N,CX,INCX,CY,INCY));
BLAS_FN		(void,	csymm,	(char *SIDE,char *UPLO,Int *M,Int*N,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int *LDC),\
       		 		(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN		(void,	csyr2k,	(char *UPLO,char *TRANS,Int *N,Int *K,Complex *ALPHA,Complex *A,Int* LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int *LDC),\
       		 		(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC));
BLAS_FN		(void,	csyrk,	(char *UPLO,char *TRANS,Int *N,Int *K,Complex *ALPHA,Complex *A,Int *LDA,Complex *BETA,Complex *C,Int *LDC),\
       		 		(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC));
BLAS_FN		(void,	ctbmv,	(char *UPLO,char *TRANS,char*DIAG,Int *N,Int *K,Complex *A,Int *LDA,Complex *X,Int *INCX),\
       		 		(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX));
BLAS_FN		(void,	ctbsv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,Int*K,Complex *A,Int *LDA,Complex *X,Int *INCX),\
       		 		(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX));
BLAS_FN		(void,	ctpmv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,Complex *AP,Complex *X,Int *INCX),\
       		 		(UPLO,TRANS,DIAG,N,AP,X,INCX));
BLAS_FN		(void,	ctpsv,	(char * UPLO,char *TRANS,char *DIAG,Int *N, Complex *AP,Complex *X,Int *INCX),\
       		 		(UPLO,TRANS,DIAG,N,AP,X,INCX));
BLAS_FN		(void,	ctrmm,	(char *SIDE,char *UPLO,char*TRANSA,char* DIAG,Int *M,Int *N,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB),\
       		 		(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB));
BLAS_FN		(void,	ctrmv,	(char *UPLO,char*TRANS,char* DIAG,Int * N,Complex *A,Int *LDA,Complex *X,Int *INCX),\
       		 		(UPLO,TRANS,DIAG,N,A,LDA,X,INCX));
BLAS_FN		(void,	ctrsm,	(char *SIDE,char*UPLO,char*TRANSA,char*DIAG,Int*M,Int*N,Complex * ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB),\
       		 		(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB));
BLAS_FN		(void,	ctrsv,	(char *UPLO,char*TRANS,char *DIAG,Int *N,Complex *A,Int *LDA,Complex *X,Int *INCX),\
				(UPLO,TRANS,DIAG,N,A,LDA,X,INCX));
#ifdef EXTBLAS 
BLAS_FN		(void,	caxpby,	(Int *N, Complex *CA, Complex *CX,Int *INCX,Complex *CB, Complex *CY,Int * INCY),\
       		 		(N,CA,CX,INCX,CB,CY,INCY)); 
BLAS_FN         (void,  comatcopy, ( char* ORDER, char* TRANS, Int *rows, Int *cols, float complex *alpha, float complex *a, Int *lda, float complex *b, Int *ldb), \
				(ORDER, TRANS, rows, cols, alpha, a, lda, b, ldb)); 
BLAS_FN         (void,  cimatcopy, ( char* ORDER, char* TRANS, Int *rows, Int *cols, float complex *alpha, float complex *a, Int *lda, Int *ldb), \
				(ORDER, TRANS, rows, cols, alpha, a, lda, ldb)); 

#endif
/* BLAS_FN(float complex,	cdotc,	(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY),\
				(N,CX,INCX,CY,INCY));
BLAS_FN(float complex,	cdotu,	(Int *N,Complex *CX,Int * INCX,Complex *CY,Int *INCY),\
				(N,CX,INCX,CY,INCY)); */
#ifdef USE_INTERFACE_INTEL 
void cdotc_(float complex *rp, Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	Complex ret = 0; 
	double ts  = 0 ;
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
	}
	if (__flexiblas_current_blas.cdotc_is_intel != 0 ) {
		void (*fn) (Complex *ret, Int *N, Complex *CX, Int *INCX, Complex *CY, Int *INCY); 
		fn = flexiblas_cdotc.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "cdotc_ not hooked, abort\n"); 
			abort(); 
		}
		fn(&ret, N, CX, INCX, CY, INCY); 
	} else {
		Complex (*fn)  (Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY); 
		fn = flexiblas_cdotc.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "cdotc_ not hooked, abort\n"); 
			abort(); 
		}
		ret = fn(N, CX, INCX, CY, INCY); 
	}
	if (__flexiblas_profile ) {
		flexiblas_time_cdotc[0] = flexiblas_wtime() - ts; 
		flexiblas_call_cdotc[0] ++; 
	}
	*rp = ret; 	
	return;  
}

void cdotc(Complex *rp, Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	cdotc_(rp,N,CX,INCX,CY,INCY); 
}

void cdotu_(Complex *rp, Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	Complex ret = 0; 
	double ts = 0;
	if ( __flexiblas_profile ){
		ts = flexiblas_wtime(); 
	}
	
	if (__flexiblas_current_blas.cdotu_is_intel != 0 ) {
		void (*fn) (Complex *ret, Int *N, Complex *CX, Int *INCX, Complex *CY, Int *INCY); 
		fn = flexiblas_cdotu.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX  "cdotu_ not hooked, abort\n"); 
			abort(); 
		}
		fn(&ret, N, CX, INCX, CY, INCY); 
	} else {
		Complex (*fn)  (Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY); 
		fn = flexiblas_cdotu.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "cdotu_ not hooked, abort\n"); 
			abort(); 
		}
		ret = fn(N, CX, INCX, CY, INCY); 
	}
	if (__flexiblas_profile ){ 
		flexiblas_time_cdotu[0] = flexiblas_wtime() - ts; 
		flexiblas_call_cdotu[0] ++; 
	}
	*rp = ret ; 
	return ; 
}

void cdotu(Complex *rp, Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	return cdotu_(rp,N,CX,INCX,CY,INCY); 
}


#else 
// The Intel MKL library can have a different calling sequence 
float complex cdotc_(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	Complex ret = 0; 
	double ts  = 0 ;
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
	}
	if (__flexiblas_current_blas.cdotc_is_intel != 0 ) {
		void (*fn) (Complex *ret, Int *N, Complex *CX, Int *INCX, Complex *CY, Int *INCY); 
		fn = flexiblas_cdotc.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "cdotc_ not hooked, abort\n"); 
			abort(); 
		}
		fn(&ret, N, CX, INCX, CY, INCY); 
	} else {
		Complex (*fn)  (Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY); 
		fn = flexiblas_cdotc.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "cdotc_ not hooked, abort\n"); 
			abort(); 
		}
		ret = fn(N, CX, INCX, CY, INCY); 
	}
	if (__flexiblas_profile ) {
		flexiblas_time_cdotc[0] = flexiblas_wtime() - ts; 
		flexiblas_call_cdotc[0] ++; 
	}
	
	return ret; 
}

float complex cdotc(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	return cdotc_(N,CX,INCX,CY,INCY); 
}

float complex cdotu_(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	Complex ret = 0; 
	double ts = 0;
	if ( __flexiblas_profile ){
		ts = flexiblas_wtime(); 
	}
	
	if (__flexiblas_current_blas.cdotu_is_intel != 0 ) {
		void (*fn) (Complex *ret, Int *N, Complex *CX, Int *INCX, Complex *CY, Int *INCY); 
		fn = flexiblas_cdotu.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX  "cdotu_ not hooked, abort\n"); 
			abort(); 
		}
		fn(&ret, N, CX, INCX, CY, INCY); 
	} else {
		Complex (*fn)  (Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY); 
		fn = flexiblas_cdotu.call_fblas; 
		if ( fn == NULL ) { 
			fprintf(stderr, PRINT_PREFIX "cdotu_ not hooked, abort\n"); 
			abort(); 
		}
		ret = fn(N, CX, INCX, CY, INCY); 
	}
	if (__flexiblas_profile ){ 
		flexiblas_time_cdotu[0] = flexiblas_wtime() - ts; 
		flexiblas_call_cdotu[0] ++; 
	}
	return ret; 
}

float complex cdotu(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY){
	return cdotu_(N,CX,INCX,CY,INCY); 
}
#endif 
