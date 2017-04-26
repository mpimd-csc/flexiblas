/* 
 Copyright (C) 2011  Martin Köhler, grisuthedragon@users.sf.net

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
#ifndef FLEXIBLAS_H
#define FLEXIBLAS_H
#include <complex.h> 
#ifndef Int
#ifndef INTEGER8 
#define Int 	int
#else 
#include <stdint.h>
#define Int 	int64_t
#endif
#endif

#ifdef BLAS_FN
#undef BLAS_FN
#endif 
#define BLAS_FN(rettype, name, args)  rettype name##_ args; 

/*-----------------------------------------------------------------------------
 *  double 
 *-----------------------------------------------------------------------------*/
BLAS_FN(double, dasum,	(Int *N, double *DX, Int *INCX)); 
BLAS_FN(void, 	daxpy,	(Int *N, double *DA, double *DX, Int * INCX, double *DY, Int *INCY)); 
BLAS_FN(double,	dcabs1,	(double complex *Z));
BLAS_FN(void,	dcopy,	(Int *N, double *DX, Int * INCX, double *DY, Int *INCY));
BLAS_FN(double,	ddot,	(Int *N, double *DX, Int * INCX, double *DY, Int *INCY));
BLAS_FN(void,	dgbmv,	(char *TRANS, Int *M, Int *N, Int *KL, Int *KU, double *ALPHA, double *A, Int *LDA, double *X, Int *INCX, double *BETA, double *Y, Int *INCY)); 
BLAS_FN(void,	dgemm,	(char * TRANSA,char *TRANSB,Int *M,Int *N,Int *K,double * ALPHA,double *A,Int * LDA,double *B,Int *LDB,double *BETA,double *C,Int *LDC)); 
BLAS_FN(void,	dgemv,	(char * TRANS, Int *M, Int *N, double *ALPHA, double *A, Int *LDA, double *X, Int *INCX, double *BETA, double *Y, Int*INCY)); 
BLAS_FN(void,	dger,	(Int *M, Int *N, double *ALPHA, double *X, Int *INCX, double *Y, Int * INCY, double *A, Int *LDA))
BLAS_FN(double,	dnrm2,	(Int *N, double *X, Int *INCX));
BLAS_FN(void,	drot, 	(Int *N, double *DX, Int *INCX, double *DY, Int* INCY, double *C, double *S));
BLAS_FN(void,	drotg,	(double *DA, double *DB, double *C, double *S));
BLAS_FN(void,	drotm,	(Int *N, double *DX, Int *INCX, double *DY, Int *INCY,double *DPARAM));
BLAS_FN(void,	drotmg,	(double *DD1, double *DD2, double *DX1, double *DY1, double *DPARAM));
BLAS_FN(void,	dsbmv,	(char *UPLO, Int *N, Int* K, double *ALPHA, double *A, Int*LDA,double *X, Int *INCX, double *BETA, double *Y, Int *INCY));
BLAS_FN(void,	dscal,	(Int *N, double *DA, double *DX, Int *INCX));
BLAS_FN(double,	dsdot,	(Int *N, float *SX, Int *INCX, float *SY, Int *INCY));
BLAS_FN(void,	dspmv,	(char *UPLO, Int *N, double *ALPHA, double *AP, double *X, Int *INCX, double *BETA, double *Y, Int *INCY)); 
BLAS_FN(void,	dspr2,	(char *UPLO, Int *N, double *ALPHA, double *X, Int *INCX, double * Y, Int *INCY, double *AP)); 
BLAS_FN(void,	dspr,	(char * UPLO,Int * N,double * ALPHA,double * X,Int * INCX,double *AP));
BLAS_FN(void,	dswap,	(Int *N, double * DX,Int * INCX,double *DY,Int *INCY));
BLAS_FN(void,	dsymm,	(char * SIDE,char * UPLO,Int *M,Int *N,double *ALPHA,double *A,Int*LDA,double*B,Int*LDB,double *BETA,double *C,Int *LDC)); 
BLAS_FN(void,	dsymv,	(char *UPLO,Int *N,double *ALPHA,double *A,Int *LDA,double *X,Int* INCX,double *BETA,double *Y,Int * INCY));
BLAS_FN(void,	dsyr2,	(char *UPLO,Int *N,double *ALPHA,double *X,Int*INCX,double*Y,Int *INCY,double *A,Int *LDA));
BLAS_FN(void,	dsyr2k,	(char *UPLO,char *TRANS,Int *N,Int *K,double * ALPHA,double * A,Int* LDA,double *B,Int *LDB,double* BETA,double *C,Int *LDC)); 
BLAS_FN(void,	dsyr,	(char *UPLO, Int *N,double *ALPHA,double*X,Int * INCX,double *A,Int *LDA)); 
BLAS_FN(void,	dsyrk,	(char* UPLO, char* TRANS,Int *N,Int *K,double *ALPHA,double *A,Int *LDA,double *BETA,double *C,Int *LDC)); 
BLAS_FN(void,	dtbmv,	(char *UPLO,char* TRANS,char*DIAG,Int *N,Int *K,double *A,Int *LDA,double *X,Int *INCX));
BLAS_FN(void,	dtbsv,	(char *UPLO,char* TRANS,char*DIAG,Int *N,Int *K,double *A,Int* LDA,double *X,Int *INCX));
BLAS_FN(void,	dtpmv,	(char *UPLO,char* TRANS,char*DIAG,Int* N,double *AP,double* X,Int*INCX));
BLAS_FN(void,	dtpsv,	(char *UPLO,char* TRANS,char*DIAG,Int* N,double *AP,double* X,Int*INCX));
BLAS_FN(void,	dtrmm,	(char *SIDE,char*UPLO,char *TRANSA,char*DIAG,Int *M,Int *N,double *ALPHA,double *A,Int *LDA,double *B,Int *LDB)); 
BLAS_FN(void,	dtrmv,	(char* UPLO,char*TRANS,char *DIAG,Int *N,double *A,Int *LDA,double *X,Int *INCX));
BLAS_FN(void,	dtrsm,	(char *SIDE,char*UPLO,char *TRANSA,char*DIAG,Int *M,Int *N,double *ALPHA,double *A,Int *LDA,double *B,Int *LDB))
BLAS_FN(void,	dtrsv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,double *A,Int *LDA,double *X,Int *INCX));
BLAS_FN(double,	dzasum,	(Int *N, double complex *ZX, Int *INCX));
BLAS_FN(double, dznrm2, (Int *N, double complex *ZX, Int *INCX));


/*-----------------------------------------------------------------------------
 *  single
 *-----------------------------------------------------------------------------*/
BLAS_FN(float, 	sasum,	(Int *N, float *DX, Int *INCX)); 
BLAS_FN(void, 	saxpy,	(Int *N, float *DA, float *DX, Int * INCX, float *DY, Int *INCY)); 
BLAS_FN(float,	scabs1,	(float complex *Z));
BLAS_FN(float,	scasum,	(Int *N, float complex *ZX, Int *INCX));
BLAS_FN(float, 	scnrm2, (Int *N, float complex *ZX, Int *INCX));
BLAS_FN(void,	scopy,	(Int *N, float *DX, Int * INCX, float *DY, Int *INCY));
BLAS_FN(float,	sdot,	(Int *N, float *DX, Int * INCX, float *DY, Int *INCY));
BLAS_FN(void,	sgbmv,	(char *TRANS, Int *M, Int *N, Int *KL, Int *KU, float *ALPHA, float *A, Int *LDA, float *X, Int *INCX, float *BETA, float *Y, Int *INCY));
BLAS_FN(void,	sgemm,	(char * TRANSA,char *TRANSB,Int *M,Int *N,Int *K,float * ALPHA,float *A,Int * LDA,float *B,Int *LDB,float *BETA,float *C,Int *LDC));
BLAS_FN(void,	sgemv,	(char * TRANS, Int *M, Int *N, float *ALPHA, float *A, Int *LDA, float *X, Int *INCX, float *BETA, float *Y, Int*INCY));
BLAS_FN(void,	sger,	(Int *M, Int *N, float *ALPHA, float *X, Int *INCX, float *Y, Int * INCY, float *A, Int *LDA));
BLAS_FN(float,	snrm2,	(Int *N, float *X, Int *INCX));
BLAS_FN(void,	srot, 	(Int *N, float *DX, Int *INCX, float *DY, Int* INCY, float *C, float *S));
BLAS_FN(void,	srotg,	(float *DA, float *DB, float *C, float *S));
BLAS_FN(void,	srotm,	(Int *N, float *DX, Int *INCX, float *DY, Int *INCY,float *DPARAM));
BLAS_FN(void,	srotmg,	(float *DD1, float *DD2, float *DX1, float *DY1, float *DPARAM));
BLAS_FN(void,	ssbmv,	(char *UPLO, Int *N, Int* K, float *ALPHA, float *A, Int *LDA, float *X, Int *INCX, float *BETA, float *Y, Int *INCY));
BLAS_FN(void,	sscal,	(Int *N, float *DA, float *DX, Int *INCX));
BLAS_FN(void,	sspmv,	(char *UPLO, Int *N, float *ALPHA, float *AP, float *X, Int *INCX, float *BETA, float *Y, Int *INCY)); 
BLAS_FN(void,	sspr2,	(char *UPLO, Int *N, float *ALPHA, float *X, Int *INCX, float * Y, Int *INCY, float *AP));
BLAS_FN(void,	sspr,	(char * UPLO,Int * N,float * ALPHA,float * X,Int * INCX,float *AP)); 
BLAS_FN(void,	sswap,	(Int *N, float * DX,Int * INCX,float *DY,Int *INCY));
BLAS_FN(void,	ssymm,	(char * SIDE,char * UPLO,Int *M,Int *N,float *ALPHA,float *A,Int*LDA,float*B,Int*LDB,float *BETA,float *C,Int *LDC)); 
BLAS_FN(void,	ssymv,	(char *UPLO,Int *N,float *ALPHA,float *A,Int *LDA,float *X,Int* INCX,float *BETA,float *Y,Int * INCY)); 
BLAS_FN(void,	ssyr2,	(char *UPLO,Int *N,float *ALPHA,float *X,Int*INCX,float*Y,Int *INCY,float *A,Int *LDA));
BLAS_FN(void,	ssyr2k,	(char *UPLO,char *TRANS,Int *N,Int *K,float * ALPHA,float * A,Int* LDA,float *B,Int *LDB,float* BETA,float *C,Int *LDC));
BLAS_FN(void,	ssyr,	(char *UPLO, Int *N,float *ALPHA,float*X,Int * INCX,float *A,Int *LDA)); 
BLAS_FN(void,	ssyrk,	(char* UPLO, char* TRANS,Int *N,Int *K,float *ALPHA,float *A,Int *LDA,float *BETA,float *C,Int *LDC)); 
BLAS_FN(void,	stbmv,	(char *UPLO,char* TRANS,char*DIAG,Int *N,Int *K,float *A,Int *LDA,float *X,Int *INCX));
BLAS_FN(void,	stbsv,	(char *UPLO,char* TRANS,char*DIAG,Int *N,Int *K,float *A,Int* LDA,float *X,Int *INCX));
BLAS_FN(void,	stpmv,	(char *UPLO,char* TRANS,char*DIAG,Int* N,float *AP,float* X,Int*INCX));
BLAS_FN(void,	stpsv,	(char *UPLO,char* TRANS,char*DIAG,Int* N,float *AP,float* X,Int*INCX));
BLAS_FN(void,	strmm,	(char *SIDE,char*UPLO,char *TRANSA,char*DIAG,Int *M,Int *N,float *ALPHA,float *A,Int *LDA,float *B,Int *LDB));
BLAS_FN(void,	strmv,	(char* UPLO,char*TRANS,char *DIAG,Int *N,float *A,Int *LDA,float *X,Int *INCX));
BLAS_FN(void,	strsm,	(char *SIDE,char*UPLO,char *TRANSA,char*DIAG,Int *M,Int *N,float *ALPHA,float *A,Int *LDA,float *B,Int *LDB)); 
BLAS_FN(void,	strsv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,float *A,Int *LDA,float *X,Int *INCX)); 

/*-----------------------------------------------------------------------------
 *  Complex * 8
 *-----------------------------------------------------------------------------*/
#ifdef Complex
#undef Complex 
#endif 
#define Complex float complex
BLAS_FN(void,	caxpy,	(Int *N, Complex *CA, Complex *CX,Int *INCX,Complex *CY,Int * INCY)); 
BLAS_FN(void,	ccopy,	(Int *N, Complex *CX,Int *INCX,Complex *CY,Int *INCY)); 
BLAS_FN(void,	cgbmv,	(char *TRANS,Int *M,Int *N,Int *KL,Int *KU,Complex *ALPHA,Complex *A,Int*LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY));
BLAS_FN(void,	cgemm,	(char *TRANSA,char*TRANSB,Int *M,Int *N,Int *K,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int*LDB,Complex *BETA,Complex *C, Int*LDC)); 
BLAS_FN(void,	cgemv,	(char * TRANS,Int *M,Int*N,Complex *ALPHA,Complex *A,Int *LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY));
BLAS_FN(void,	cgerc,	(Int *M, Int*N,Complex *ALPHA,Complex*X,Int*INCX,Complex *Y,Int *INCY,Complex *A,Int *LDA));
BLAS_FN(void,	cgeru,	(Int *M,Int *N,Complex *ALPHA,Complex *X, Int *INCX,Complex *Y,Int *INCY,Complex* A,Int *LDA));
BLAS_FN(void,	chbmv,	(char* UPLO,Int *N,Int*K,Complex *ALPHA,Complex*A,Int*LDA,Complex * X,Int * INCX,Complex *BETA,Complex *Y,Int* INCY));
BLAS_FN(void,	chemm,	(char* SIDE,char*UPLO,Int *M,Int*N,Complex*ALPHA,Complex*A,Int *LDA,Complex *B,Int *LDB,Complex * BETA,Complex *C,Int *LDC));
BLAS_FN(void,	chemv,	(char*UPLO,Int*N,Complex *ALPHA,Complex*A,Int*LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY));
BLAS_FN(void,	cher2,	(char *UPLO,Int *N,Complex * ALPHA,Complex *X,Int *INCX,Complex *Y,Int *INCY,Complex *A,Int *LDA));
BLAS_FN(void,	cher2k,	(char *UPLO,char *TRANS,Int *N,Int*K,Complex*ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int*LDC));
BLAS_FN(void,	cher,	(char *UPLO,Int *N,Complex *ALPHA,Complex *X,Int *INCX,Complex *A,Int *LDA));
BLAS_FN(void,	cherk,	(char *UPLO,char *TRANS,Int *N,Int*K,Complex *ALPHA,Complex *A,Int *LDA,Complex *BETA,Complex*C,Int *LDC));
BLAS_FN(void,	chpmv,	(char *UPLO,Int *N,Complex *ALPHA,Complex *AP,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY));
BLAS_FN(void,	chpr2,	(char *UPLO,Int *N, Complex *ALPHA,Complex *X,Int *INCX,Complex *Y,Int *INCY,Complex *AP));
BLAS_FN(void,	chpr,	(char *UPLO,Int *N,Complex *ALPHA,Complex *X,Int*INCX,Complex *AP));
BLAS_FN(void,	crotg,	(Complex *CA,Complex *CB,float *C,Complex *S));
BLAS_FN(void,	cscal,	(Int *N,Complex *CA,Complex *CX,Int *INCX));
BLAS_FN(void,	csrot,	(Int * N, Complex *CX, Int * INCX, Complex *CY, Int * INCY, float *C, float *S ));
BLAS_FN(void,	csscal,	(Int *N,float *SA,Complex *CX,Int * INCX));
BLAS_FN(void,	cswap,	(Int *N,Complex *CX,Int *INCX,Complex *CY,Int *INCY));
BLAS_FN(void,	csymm,	(char *SIDE,char *UPLO,Int *M,Int*N,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int *LDC));
BLAS_FN(void,	csyr2k,	(char *UPLO,char *TRANS,Int *N,Int *K,Complex *ALPHA,Complex *A,Int* LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int *LDC));
BLAS_FN(void,	csyrk,	(char *UPLO,char *TRANS,Int *N,Int *K,Complex *ALPHA,Complex *A,Int *LDA,Complex *BETA,Complex *C,Int *LDC));
BLAS_FN(void,	ctbmv,	(char *UPLO,char *TRANS,char*DIAG,Int *N,Int *K,Complex *A,Int *LDA,Complex *X,Int *INCX));
BLAS_FN(void,	ctbsv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,Int*K,Complex *A,Int *LDA,Complex *X,Int *INCX));
BLAS_FN(void,	ctpmv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,Complex *AP,Complex *X,Int *INCX));
BLAS_FN(void,	ctpsv,	(char * UPLO,char *TRANS,char *DIAG,Int *M, Complex *AP,Complex *X,Int *INCX));
BLAS_FN(void,	ctrmm,	(char *SIDE,char *UPLO,char*TRANSA,char* DIAG,Int *M,Int *N,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB));
BLAS_FN(void,	ctrmv,	(char *UPLO,char*TRANS,char* DIAG,Int * N,Complex *A,Int *LDA,Complex *X,Int *INCX));
BLAS_FN(void,	ctrsm,	(char *SIDE,char*UPLO,char*TRANSA,char*DIAG,Int*M,Int*N,Complex * ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB));
BLAS_FN(void,	ctrsv,	(char *UPLO,char*TRANS,char *DIAG,Int *N,Complex *A,Int *LDA,Complex *X,Int *INCX));
BLAS_FN(float complex,	cdotc,	(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY));
BLAS_FN(float complex,	cdotu,	(Int *N,Complex *CX,Int * INCX,Complex *CY,Int *INCY));

/*-----------------------------------------------------------------------------
 *  Complex * 16
 *-----------------------------------------------------------------------------*/
#ifdef Complex
#undef Complex 
#endif 
#define Complex double complex
BLAS_FN(void,	zaxpy,	(Int *N, Complex *CA, Complex *CX,Int *INCX,Complex *CY,Int * INCY)); 
BLAS_FN(void,	zcopy,	(Int *N, Complex *CX,Int *INCX,Complex *CY,Int *INCY)); 
BLAS_FN(void,	zgbmv,	(char *TRANS,Int *M,Int *N,Int *KL,Int *KU,Complex *ALPHA,Complex *A,Int*LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY));
BLAS_FN(void,	zgemm,	(char *TRANSA,char*TRANSB,Int *M,Int *N,Int *K,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int*LDB,Complex *BETA,Complex *C, Int*LDC)); 
BLAS_FN(void,	zgemv,	(char * TRANS,Int *M,Int*N,Complex *ALPHA,Complex *A,Int *LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY));
BLAS_FN(void,	zgerc,	(Int *M, Int*N,Complex *ALPHA,Complex*X,Int*INCX,Complex *Y,Int *INCY,Complex *A,Int *LDA));
BLAS_FN(void,	zgeru,	(Int *M,Int *N,Complex *ALPHA,Complex *X, Int *INCX,Complex *Y,Int *INCY,Complex* A,Int *LDA));
BLAS_FN(void,	zhbmv,	(char* UPLO,Int *N,Int*K,Complex *ALPHA,Complex*A,Int*LDA,Complex * X,Int * INCX,Complex *BETA,Complex *Y,Int* INCY));
BLAS_FN(void,	zhemm,	(char* SIDE,char*UPLO,Int *M,Int*N,Complex*ALPHA,Complex*A,Int *LDA,Complex *B,Int *LDB,Complex * BETA,Complex *C,Int *LDC));
BLAS_FN(void,	zhemv,	(char*UPLO,Int*N,Complex *ALPHA,Complex*A,Int*LDA,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY));
BLAS_FN(void,	zher2,	(char *UPLO,Int *N,Complex * ALPHA,Complex *X,Int *INCX,Complex *Y,Int *INCY,Complex *A,Int *LDA));
BLAS_FN(void,	zher2k,	(char *UPLO,char *TRANS,Int *N,Int*K,Complex*ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int*LDC));
BLAS_FN(void,	zher,	(char *UPLO,Int *N,Complex *ALPHA,Complex *X,Int *INCX,Complex *A,Int *LDA));
BLAS_FN(void,	zherk,	(char *UPLO,char *TRANS,Int *N,Int*K,Complex *ALPHA,Complex *A,Int *LDA,Complex *BETA,Complex*C,Int *LDC));
BLAS_FN(void,	zhpmv,	(char *UPLO,Int *N,Complex *ALPHA,Complex *AP,Complex *X,Int *INCX,Complex *BETA,Complex *Y,Int *INCY));
BLAS_FN(void,	zhpr2,	(char *UPLO,Int *N, Complex *ALPHA,Complex *X,Int *INCX,Complex *Y,Int *INCY,Complex *AP));
BLAS_FN(void,	zhpr,	(char *UPLO,Int *N,Complex *ALPHA,Complex *X,Int*INCX,Complex *AP));
BLAS_FN(void,	zrotg,	(Complex *CA,Complex *CB,double *C,Complex *S));
BLAS_FN(void,	zscal,	(Int *N,Complex *CA,Complex *CX,Int *INCX));
BLAS_FN(void,	zdrot,	(Int * N, Complex *CX, Int * INCX, Complex *CY, Int * INCY, double *C, double *S ));
BLAS_FN(void,	zdscal,	(Int *N,double *SA,Complex *CX,Int * INCX));
BLAS_FN(void,	zswap,	(Int *N,Complex *CX,Int *INCX,Complex *CY,Int *INCY));
BLAS_FN(void,	zsymm,	(char *SIDE,char *UPLO,Int *M,Int*N,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int *LDC));
BLAS_FN(void,	zsyr2k,	(char *UPLO,char *TRANS,Int *N,Int *K,Complex *ALPHA,Complex *A,Int* LDA,Complex *B,Int *LDB,Complex *BETA,Complex *C,Int *LDC));
BLAS_FN(void,	zsyrk,	(char *UPLO,char *TRANS,Int *N,Int *K,Complex *ALPHA,Complex *A,Int *LDA,Complex *BETA,Complex *C,Int *LDC));
BLAS_FN(void,	ztbmv,	(char *UPLO,char *TRANS,char*DIAG,Int *N,Int *K,Complex *A,Int *LDA,Complex *X,Int *INCX));
BLAS_FN(void,	ztbsv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,Int*K,Complex *A,Int *LDA,Complex *X,Int *INCX));
BLAS_FN(void,	ztpmv,	(char *UPLO,char *TRANS,char *DIAG,Int *N,Complex *AP,Complex *X,Int *INCX));
BLAS_FN(void,	ztpsv,	(char * UPLO,char *TRANS,char *DIAG,Int *M, Complex *AP,Complex *X,Int *INCX));
BLAS_FN(void,	ztrmm,	(char *SIDE,char *UPLO,char*TRANSA,char* DIAG,Int *M,Int *N,Complex *ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB));
BLAS_FN(void,	ztrmv,	(char *UPLO,char*TRANS,char* DIAG,Int * N,Complex *A,Int *LDA,Complex *X,Int *INCX));
BLAS_FN(void,	ztrsm,	(char *SIDE,char*UPLO,char*TRANSA,char*DIAG,Int*M,Int*N,Complex * ALPHA,Complex *A,Int *LDA,Complex *B,Int *LDB));
BLAS_FN(void,	ztrsv,	(char *UPLO,char*TRANS,char *DIAG,Int *N,Complex *A,Int *LDA,Complex *X,Int *INCX));
BLAS_FN(double complex,	zdotc,	(Int * N,Complex *CX,Int *INCX,Complex *CY,Int *INCY));
BLAS_FN(double complex,	zdotu,	(Int *N,Complex *CX,Int * INCX,Complex *CY,Int *INCY));

/*-----------------------------------------------------------------------------
 *  int 
 *-----------------------------------------------------------------------------*/
BLAS_FN(Int,icamax,(Int *N, float complex *CX, Int *INCX));
BLAS_FN(Int,idamax,(Int *N, double *CX, Int *INCX));
BLAS_FN(Int,isamax,(Int *N, float *CX, Int *INCX));
BLAS_FN(Int,izamax,(Int *N, double complex *CX, Int *INCX));

#undef BLAS_FN

#endif

