#ifndef EXTBLAS_H
#define EXTBLAS_H

#include <stdint.h>
#include <complex.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(USE_BLAS_32) && defined(USE_BLAS_64)
#error Either USE_BLAS_32 or USE_BLAS_64 must be defined!
#endif

#ifdef USE_BLAS_32
#define blasint int32_t
#define CAXPBY   caxpby32_
#define DAXPBY   daxpby32_
#define ZAXPBY   zaxpby32_
#define SAXPBY   saxpby32_
#define COMATCOPY comatcopy32_
#define ZOMATCOPY zomatcopy32_
#define DOMATCOPY domatcopy32_
#define SOMATCOPY somatcopy32_
#define CIMATCOPY cimatcopy32_
#define ZIMATCOPY zimatcopy32_
#define DIMATCOPY dimatcopy32_
#define SIMATCOPY simatcopy32_
#define SGEADD   sgeadd32_
#define DGEADD   dgeadd32_
#define CGEADD   cgeadd32_
#define ZGEADD   zgeadd32_
#endif

#ifdef USE_BLAS_64
#define blasint int64_t
#define CAXPBY   caxpby64_
#define DAXPBY   daxpby64_
#define ZAXPBY   zaxpby64_
#define SAXPBY   saxpby64_
#define COMATCOPY comatcopy64_
#define ZOMATCOPY zomatcopy64_
#define DOMATCOPY domatcopy64_
#define SOMATCOPY somatcopy64_
#define CIMATCOPY cimatcopy64_
#define ZIMATCOPY zimatcopy64_
#define DIMATCOPY dimatcopy64_
#define SIMATCOPY simatcopy64_
#define SGEADD   sgeadd64_
#define DGEADD   dgeadd64_
#define CGEADD   cgeadd64_
#define ZGEADD   zgeadd64_
#endif

#ifndef blasint 
#define blasint int
#define CAXPBY   caxpby_ 
#define DAXPBY   daxpby_ 
#define ZAXPBY   zaxpby_ 
#define SAXPBY   saxpby_ 
#define COMATCOPY comatcopy_
#define ZOMATCOPY zomatcopy_
#define DOMATCOPY domatcopy_
#define SOMATCOPY somatcopy_
#define CIMATCOPY cimatcopy_
#define ZIMATCOPY zimatcopy_
#define DIMATCOPY dimatcopy_
#define SIMATCOPY simatcopy_
#define SGEADD   sgeadd_ 
#define DGEADD   dgeadd_ 
#define CGEADD   cgeadd_ 
#define ZGEADD   zgeadd_ 
#endif

void caxpby_(blasint* n, float complex* ca, float complex* cx, blasint* incx, float complex* cb, float complex* cy, blasint* incy);
void caxpby32_(int32_t* n, float complex* ca, float complex* cx, int32_t* incx, float complex* cb, float complex* cy, int32_t* incy);
void caxpby64_(int64_t* n, float complex* ca, float complex* cx, int64_t* incx, float complex* cb, float complex* cy, int64_t* incy);

void daxpby_(blasint* n, double* da, double* dx, blasint* incx, double* db, double* dy, blasint* incy);
void daxpby32_(int32_t* n, double* da, double* dx, int32_t* incx, double* db, double* dy, int32_t* incy);
void daxpby64_(int64_t* n, double* da, double* dx, int64_t* incx, double* db, double* dy, int64_t* incy);

void zaxpby_(blasint* n, double complex* za, double complex* zx, blasint* incx, double complex* zb, double complex* zy, blasint* incy);
void zaxpby32_(int32_t* n, double complex* za, double complex* zx, int32_t* incx, double complex* zb, double complex* zy, int32_t* incy);
void zaxpby64_(int64_t* n, double complex* za, double complex* zx, int64_t* incx, double complex* zb, double complex* zy, int64_t* incy);

void saxpby_(blasint* n, float* sa, float* sx, blasint* incx, float* sb, float* sy, blasint* incy);
void saxpby32_(int32_t* n, float* sa, float* sx, int32_t* incx, float* sb, float* sy, int32_t* incy);
void saxpby64_(int64_t* n, float* sa, float* sx, int64_t* incx, float* sb, float* sy, int64_t* incy);

void comatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb);
void comatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, float complex* alpha, float complex* a, int32_t* lda, float complex* b, int32_t* ldb);
void comatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, float complex* alpha, float complex* a, int64_t* lda, float complex* b, int64_t* ldb);

void zomatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb);
void zomatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, double complex* alpha, double complex* a, int32_t* lda, double complex* b, int32_t* ldb);
void zomatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, double complex* alpha, double complex* a, int64_t* lda, double complex* b, int64_t* ldb);

void domatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, double* b, blasint* ldb);
void domatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, double* alpha, double* a, int32_t* lda, double* b, int32_t* ldb);
void domatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, double* alpha, double* a, int64_t* lda, double* b, int64_t* ldb);

void somatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, float* b, blasint* ldb);
void somatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, float* alpha, float* a, int32_t* lda, float* b, int32_t* ldb);
void somatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, float* alpha, float* a, int64_t* lda, float* b, int64_t* ldb);

void cimatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float complex* alpha, float complex* a, blasint* lda, blasint* ldb);
void cimatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, float complex* alpha, float complex* a, int32_t* lda, int32_t* ldb);
void cimatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, float complex* alpha, float complex* a, int64_t* lda, int64_t* ldb);

void zimatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double complex* alpha, double complex* a, blasint* lda, blasint* ldb);
void zimatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, double complex* alpha, double complex* a, int32_t* lda, int32_t* ldb);
void zimatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, double complex* alpha, double complex* a, int64_t* lda, int64_t* ldb);

void dimatcopy_(char* order, char* trans, blasint* rows, blasint* cols, double* alpha, double* a, blasint* lda, blasint* ldb);
void dimatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, double* alpha, double* a, int32_t* lda, int32_t* ldb);
void dimatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, double* alpha, double* a, int64_t* lda, int64_t* ldb);

void simatcopy_(char* order, char* trans, blasint* rows, blasint* cols, float* alpha, float* a, blasint* lda, blasint* ldb);
void simatcopy32_(char* order, char* trans, int32_t* rows, int32_t* cols, float* alpha, float* a, int32_t* lda, int32_t* ldb);
void simatcopy64_(char* order, char* trans, int64_t* rows, int64_t* cols, float* alpha, float* a, int64_t* lda, int64_t* ldb);

void sgeadd_(blasint* m, blasint* n, float* alpha, float* a, blasint* lda, float* beta, float* b, blasint* ldb);
void sgeadd32_(int32_t* m, int32_t* n, float* alpha, float* a, int32_t* lda, float* beta, float* b, int32_t* ldb);
void sgeadd64_(int64_t* m, int64_t* n, float* alpha, float* a, int64_t* lda, float* beta, float* b, int64_t* ldb);

void dgeadd_(blasint* m, blasint* n, double* alpha, double* a, blasint* lda, double* beta, double* b, blasint* ldb);
void dgeadd32_(int32_t* m, int32_t* n, double* alpha, double* a, int32_t* lda, double* beta, double* b, int32_t* ldb);
void dgeadd64_(int64_t* m, int64_t* n, double* alpha, double* a, int64_t* lda, double* beta, double* b, int64_t* ldb);

void cgeadd_(blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* beta, float complex* b, blasint* ldb);
void cgeadd32_(int32_t* m, int32_t* n, float complex* alpha, float complex* a, int32_t* lda, float complex* beta, float complex* b, int32_t* ldb);
void cgeadd64_(int64_t* m, int64_t* n, float complex* alpha, float complex* a, int64_t* lda, float complex* beta, float complex* b, int64_t* ldb);

void zgeadd_(blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* beta, double complex* b, blasint* ldb);
void zgeadd32_(int32_t* m, int32_t* n, double complex* alpha, double complex* a, int32_t* lda, double complex* beta, double complex* b, int32_t* ldb);
void zgeadd64_(int64_t* m, int64_t* n, double complex* alpha, double complex* a, int64_t* lda, double complex* beta, double complex* b, int64_t* ldb);


#ifdef __cplusplus
}
#endif
#endif
