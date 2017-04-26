#ifndef BLAS_H
#define BLAS_H

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
#define CAXPY    caxpy32_
#define CCOPY    ccopy32_
#define CDOTC    cdotc32_
#define CDOTU    cdotu32_
#define CGBMV    cgbmv32_
#define CGEMM    cgemm32_
#define CGEMV    cgemv32_
#define CGERC    cgerc32_
#define CGERU    cgeru32_
#define CHBMV    chbmv32_
#define CHEMM    chemm32_
#define CHEMV    chemv32_
#define CHER     cher32_ 
#define CHER2    cher232_
#define CHER2K   cher2k32_
#define CHERK    cherk32_
#define CHPMV    chpmv32_
#define CHPR     chpr32_ 
#define CHPR2    chpr232_
#define CROTG    crotg32_
#define CSCAL    cscal32_
#define CSROT    csrot32_
#define CSSCAL   csscal32_
#define CSWAP    cswap32_
#define CSYMM    csymm32_
#define CSYR2K   csyr2k32_
#define CSYRK    csyrk32_
#define CTBMV    ctbmv32_
#define CTBSV    ctbsv32_
#define CTPMV    ctpmv32_
#define CTPSV    ctpsv32_
#define CTRMM    ctrmm32_
#define CTRMV    ctrmv32_
#define CTRSM    ctrsm32_
#define CTRSV    ctrsv32_
#define DASUM    dasum32_
#define DAXPY    daxpy32_
#define DCOPY    dcopy32_
#define DDOT     ddot32_ 
#define DGBMV    dgbmv32_
#define DGEMM    dgemm32_
#define DGEMV    dgemv32_
#define DGER     dger32_ 
#define DNRM2    dnrm232_
#define DROT     drot32_ 
#define DROTG    drotg32_
#define DROTM    drotm32_
#define DROTMG   drotmg32_
#define DSBMV    dsbmv32_
#define DSCAL    dscal32_
#define DSDOT    dsdot32_
#define DSPMV    dspmv32_
#define DSPR     dspr32_ 
#define DSPR2    dspr232_
#define DSWAP    dswap32_
#define DSYMM    dsymm32_
#define DSYMV    dsymv32_
#define DSYR     dsyr32_ 
#define DSYR2    dsyr232_
#define DSYR2K   dsyr2k32_
#define DSYRK    dsyrk32_
#define DTBMV    dtbmv32_
#define DTBSV    dtbsv32_
#define DTPMV    dtpmv32_
#define DTPSV    dtpsv32_
#define DTRMM    dtrmm32_
#define DTRMV    dtrmv32_
#define DTRSM    dtrsm32_
#define DTRSV    dtrsv32_
#define DZASUM   dzasum32_
#define DZNRM2   dznrm232_
#define ICAMAX   icamax32_
#define IDAMAX   idamax32_
#define ISAMAX   isamax32_
#define IZAMAX   izamax32_
#define SASUM    sasum32_
#define SAXPY    saxpy32_
#define SCASUM   scasum32_
#define SCNRM2   scnrm232_
#define SCOPY    scopy32_
#define SDOT     sdot32_ 
#define SDSDOT   sdsdot32_
#define SGBMV    sgbmv32_
#define SGEMM    sgemm32_
#define SGEMV    sgemv32_
#define SGER     sger32_ 
#define SNRM2    snrm232_
#define SROT     srot32_ 
#define SROTG    srotg32_
#define SROTM    srotm32_
#define SROTMG   srotmg32_
#define SSBMV    ssbmv32_
#define SSCAL    sscal32_
#define SSPMV    sspmv32_
#define SSPR     sspr32_ 
#define SSPR2    sspr232_
#define SSWAP    sswap32_
#define SSYMM    ssymm32_
#define SSYMV    ssymv32_
#define SSYR     ssyr32_ 
#define SSYR2    ssyr232_
#define SSYR2K   ssyr2k32_
#define SSYRK    ssyrk32_
#define STBMV    stbmv32_
#define STBSV    stbsv32_
#define STPMV    stpmv32_
#define STPSV    stpsv32_
#define STRMM    strmm32_
#define STRMV    strmv32_
#define STRSM    strsm32_
#define STRSV    strsv32_
#define ZAXPY    zaxpy32_
#define ZCOPY    zcopy32_
#define ZDOTC    zdotc32_
#define ZDOTU    zdotu32_
#define ZDROT    zdrot32_
#define ZDSCAL   zdscal32_
#define ZGBMV    zgbmv32_
#define ZGEMM    zgemm32_
#define ZGEMV    zgemv32_
#define ZGERC    zgerc32_
#define ZGERU    zgeru32_
#define ZHBMV    zhbmv32_
#define ZHEMM    zhemm32_
#define ZHEMV    zhemv32_
#define ZHER     zher32_ 
#define ZHER2    zher232_
#define ZHER2K   zher2k32_
#define ZHERK    zherk32_
#define ZHPMV    zhpmv32_
#define ZHPR     zhpr32_ 
#define ZHPR2    zhpr232_
#define ZROTG    zrotg32_
#define ZSCAL    zscal32_
#define ZSWAP    zswap32_
#define ZSYMM    zsymm32_
#define ZSYR2K   zsyr2k32_
#define ZSYRK    zsyrk32_
#define ZTBMV    ztbmv32_
#define ZTBSV    ztbsv32_
#define ZTPMV    ztpmv32_
#define ZTPSV    ztpsv32_
#define ZTRMM    ztrmm32_
#define ZTRMV    ztrmv32_
#define ZTRSM    ztrsm32_
#define ZTRSV    ztrsv32_
#endif

#ifdef USE_BLAS_64
#define blasint int64_t
#define CAXPY    caxpy64_
#define CCOPY    ccopy64_
#define CDOTC    cdotc64_
#define CDOTU    cdotu64_
#define CGBMV    cgbmv64_
#define CGEMM    cgemm64_
#define CGEMV    cgemv64_
#define CGERC    cgerc64_
#define CGERU    cgeru64_
#define CHBMV    chbmv64_
#define CHEMM    chemm64_
#define CHEMV    chemv64_
#define CHER     cher64_ 
#define CHER2    cher264_
#define CHER2K   cher2k64_
#define CHERK    cherk64_
#define CHPMV    chpmv64_
#define CHPR     chpr64_ 
#define CHPR2    chpr264_
#define CROTG    crotg64_
#define CSCAL    cscal64_
#define CSROT    csrot64_
#define CSSCAL   csscal64_
#define CSWAP    cswap64_
#define CSYMM    csymm64_
#define CSYR2K   csyr2k64_
#define CSYRK    csyrk64_
#define CTBMV    ctbmv64_
#define CTBSV    ctbsv64_
#define CTPMV    ctpmv64_
#define CTPSV    ctpsv64_
#define CTRMM    ctrmm64_
#define CTRMV    ctrmv64_
#define CTRSM    ctrsm64_
#define CTRSV    ctrsv64_
#define DASUM    dasum64_
#define DAXPY    daxpy64_
#define DCOPY    dcopy64_
#define DDOT     ddot64_ 
#define DGBMV    dgbmv64_
#define DGEMM    dgemm64_
#define DGEMV    dgemv64_
#define DGER     dger64_ 
#define DNRM2    dnrm264_
#define DROT     drot64_ 
#define DROTG    drotg64_
#define DROTM    drotm64_
#define DROTMG   drotmg64_
#define DSBMV    dsbmv64_
#define DSCAL    dscal64_
#define DSDOT    dsdot64_
#define DSPMV    dspmv64_
#define DSPR     dspr64_ 
#define DSPR2    dspr264_
#define DSWAP    dswap64_
#define DSYMM    dsymm64_
#define DSYMV    dsymv64_
#define DSYR     dsyr64_ 
#define DSYR2    dsyr264_
#define DSYR2K   dsyr2k64_
#define DSYRK    dsyrk64_
#define DTBMV    dtbmv64_
#define DTBSV    dtbsv64_
#define DTPMV    dtpmv64_
#define DTPSV    dtpsv64_
#define DTRMM    dtrmm64_
#define DTRMV    dtrmv64_
#define DTRSM    dtrsm64_
#define DTRSV    dtrsv64_
#define DZASUM   dzasum64_
#define DZNRM2   dznrm264_
#define ICAMAX   icamax64_
#define IDAMAX   idamax64_
#define ISAMAX   isamax64_
#define IZAMAX   izamax64_
#define SASUM    sasum64_
#define SAXPY    saxpy64_
#define SCASUM   scasum64_
#define SCNRM2   scnrm264_
#define SCOPY    scopy64_
#define SDOT     sdot64_ 
#define SDSDOT   sdsdot64_
#define SGBMV    sgbmv64_
#define SGEMM    sgemm64_
#define SGEMV    sgemv64_
#define SGER     sger64_ 
#define SNRM2    snrm264_
#define SROT     srot64_ 
#define SROTG    srotg64_
#define SROTM    srotm64_
#define SROTMG   srotmg64_
#define SSBMV    ssbmv64_
#define SSCAL    sscal64_
#define SSPMV    sspmv64_
#define SSPR     sspr64_ 
#define SSPR2    sspr264_
#define SSWAP    sswap64_
#define SSYMM    ssymm64_
#define SSYMV    ssymv64_
#define SSYR     ssyr64_ 
#define SSYR2    ssyr264_
#define SSYR2K   ssyr2k64_
#define SSYRK    ssyrk64_
#define STBMV    stbmv64_
#define STBSV    stbsv64_
#define STPMV    stpmv64_
#define STPSV    stpsv64_
#define STRMM    strmm64_
#define STRMV    strmv64_
#define STRSM    strsm64_
#define STRSV    strsv64_
#define ZAXPY    zaxpy64_
#define ZCOPY    zcopy64_
#define ZDOTC    zdotc64_
#define ZDOTU    zdotu64_
#define ZDROT    zdrot64_
#define ZDSCAL   zdscal64_
#define ZGBMV    zgbmv64_
#define ZGEMM    zgemm64_
#define ZGEMV    zgemv64_
#define ZGERC    zgerc64_
#define ZGERU    zgeru64_
#define ZHBMV    zhbmv64_
#define ZHEMM    zhemm64_
#define ZHEMV    zhemv64_
#define ZHER     zher64_ 
#define ZHER2    zher264_
#define ZHER2K   zher2k64_
#define ZHERK    zherk64_
#define ZHPMV    zhpmv64_
#define ZHPR     zhpr64_ 
#define ZHPR2    zhpr264_
#define ZROTG    zrotg64_
#define ZSCAL    zscal64_
#define ZSWAP    zswap64_
#define ZSYMM    zsymm64_
#define ZSYR2K   zsyr2k64_
#define ZSYRK    zsyrk64_
#define ZTBMV    ztbmv64_
#define ZTBSV    ztbsv64_
#define ZTPMV    ztpmv64_
#define ZTPSV    ztpsv64_
#define ZTRMM    ztrmm64_
#define ZTRMV    ztrmv64_
#define ZTRSM    ztrsm64_
#define ZTRSV    ztrsv64_
#endif

#ifndef blasint 
#define blasint int
#define CAXPY    caxpy_  
#define CCOPY    ccopy_  
#define CDOTC    cdotc_  
#define CDOTU    cdotu_  
#define CGBMV    cgbmv_  
#define CGEMM    cgemm_  
#define CGEMV    cgemv_  
#define CGERC    cgerc_  
#define CGERU    cgeru_  
#define CHBMV    chbmv_  
#define CHEMM    chemm_  
#define CHEMV    chemv_  
#define CHER     cher_   
#define CHER2    cher2_  
#define CHER2K   cher2k_ 
#define CHERK    cherk_  
#define CHPMV    chpmv_  
#define CHPR     chpr_   
#define CHPR2    chpr2_  
#define CROTG    crotg_  
#define CSCAL    cscal_  
#define CSROT    csrot_  
#define CSSCAL   csscal_ 
#define CSWAP    cswap_  
#define CSYMM    csymm_  
#define CSYR2K   csyr2k_ 
#define CSYRK    csyrk_  
#define CTBMV    ctbmv_  
#define CTBSV    ctbsv_  
#define CTPMV    ctpmv_  
#define CTPSV    ctpsv_  
#define CTRMM    ctrmm_  
#define CTRMV    ctrmv_  
#define CTRSM    ctrsm_  
#define CTRSV    ctrsv_  
#define DASUM    dasum_  
#define DAXPY    daxpy_  
#define DCOPY    dcopy_  
#define DDOT     ddot_   
#define DGBMV    dgbmv_  
#define DGEMM    dgemm_  
#define DGEMV    dgemv_  
#define DGER     dger_   
#define DNRM2    dnrm2_  
#define DROT     drot_   
#define DROTG    drotg_  
#define DROTM    drotm_  
#define DROTMG   drotmg_ 
#define DSBMV    dsbmv_  
#define DSCAL    dscal_  
#define DSDOT    dsdot_  
#define DSPMV    dspmv_  
#define DSPR     dspr_   
#define DSPR2    dspr2_  
#define DSWAP    dswap_  
#define DSYMM    dsymm_  
#define DSYMV    dsymv_  
#define DSYR     dsyr_   
#define DSYR2    dsyr2_  
#define DSYR2K   dsyr2k_ 
#define DSYRK    dsyrk_  
#define DTBMV    dtbmv_  
#define DTBSV    dtbsv_  
#define DTPMV    dtpmv_  
#define DTPSV    dtpsv_  
#define DTRMM    dtrmm_  
#define DTRMV    dtrmv_  
#define DTRSM    dtrsm_  
#define DTRSV    dtrsv_  
#define DZASUM   dzasum_ 
#define DZNRM2   dznrm2_ 
#define ICAMAX   icamax_ 
#define IDAMAX   idamax_ 
#define ISAMAX   isamax_ 
#define IZAMAX   izamax_ 
#define SASUM    sasum_  
#define SAXPY    saxpy_  
#define SCASUM   scasum_ 
#define SCNRM2   scnrm2_ 
#define SCOPY    scopy_  
#define SDOT     sdot_   
#define SDSDOT   sdsdot_ 
#define SGBMV    sgbmv_  
#define SGEMM    sgemm_  
#define SGEMV    sgemv_  
#define SGER     sger_   
#define SNRM2    snrm2_  
#define SROT     srot_   
#define SROTG    srotg_  
#define SROTM    srotm_  
#define SROTMG   srotmg_ 
#define SSBMV    ssbmv_  
#define SSCAL    sscal_  
#define SSPMV    sspmv_  
#define SSPR     sspr_   
#define SSPR2    sspr2_  
#define SSWAP    sswap_  
#define SSYMM    ssymm_  
#define SSYMV    ssymv_  
#define SSYR     ssyr_   
#define SSYR2    ssyr2_  
#define SSYR2K   ssyr2k_ 
#define SSYRK    ssyrk_  
#define STBMV    stbmv_  
#define STBSV    stbsv_  
#define STPMV    stpmv_  
#define STPSV    stpsv_  
#define STRMM    strmm_  
#define STRMV    strmv_  
#define STRSM    strsm_  
#define STRSV    strsv_  
#define ZAXPY    zaxpy_  
#define ZCOPY    zcopy_  
#define ZDOTC    zdotc_  
#define ZDOTU    zdotu_  
#define ZDROT    zdrot_  
#define ZDSCAL   zdscal_ 
#define ZGBMV    zgbmv_  
#define ZGEMM    zgemm_  
#define ZGEMV    zgemv_  
#define ZGERC    zgerc_  
#define ZGERU    zgeru_  
#define ZHBMV    zhbmv_  
#define ZHEMM    zhemm_  
#define ZHEMV    zhemv_  
#define ZHER     zher_   
#define ZHER2    zher2_  
#define ZHER2K   zher2k_ 
#define ZHERK    zherk_  
#define ZHPMV    zhpmv_  
#define ZHPR     zhpr_   
#define ZHPR2    zhpr2_  
#define ZROTG    zrotg_  
#define ZSCAL    zscal_  
#define ZSWAP    zswap_  
#define ZSYMM    zsymm_  
#define ZSYR2K   zsyr2k_ 
#define ZSYRK    zsyrk_  
#define ZTBMV    ztbmv_  
#define ZTBSV    ztbsv_  
#define ZTPMV    ztpmv_  
#define ZTPSV    ztpsv_  
#define ZTRMM    ztrmm_  
#define ZTRMV    ztrmv_  
#define ZTRSM    ztrsm_  
#define ZTRSV    ztrsv_  
#endif

void caxpy_(blasint* n, float complex* ca, float complex* cx, blasint* incx, float complex* cy, blasint* incy);
void caxpy32_(int32_t* n, float complex* ca, float complex* cx, int32_t* incx, float complex* cy, int32_t* incy);
void caxpy64_(int64_t* n, float complex* ca, float complex* cx, int64_t* incx, float complex* cy, int64_t* incy);

void ccopy_(blasint* n, float complex* cx, blasint* incx, float complex* cy, blasint* incy);
void ccopy32_(int32_t* n, float complex* cx, int32_t* incx, float complex* cy, int32_t* incy);
void ccopy64_(int64_t* n, float complex* cx, int64_t* incx, float complex* cy, int64_t* incy);

void cdotc_( float complex* returnvalue, blasint* n, float complex* cx, blasint* incx, float complex* cy, blasint* incy);
void cdotc32_( float complex* returnvalue, int32_t* n, float complex* cx, int32_t* incx, float complex* cy, int32_t* incy);
void cdotc64_( float complex* returnvalue, int64_t* n, float complex* cx, int64_t* incx, float complex* cy, int64_t* incy);

void cdotu_( float complex* returnvalue, blasint* n, float complex* cx, blasint* incx, float complex* cy, blasint* incy);
void cdotu32_( float complex* returnvalue, int32_t* n, float complex* cx, int32_t* incx, float complex* cy, int32_t* incy);
void cdotu64_( float complex* returnvalue, int64_t* n, float complex* cx, int64_t* incx, float complex* cy, int64_t* incy);

void cgbmv_(char* trans, blasint* m, blasint* n, blasint* kl, blasint* ku, float complex* alpha, float complex* a, blasint* lda, float complex* x, blasint* incx, float complex* beta, float complex* y, blasint* incy);
void cgbmv32_(char* trans, int32_t* m, int32_t* n, int32_t* kl, int32_t* ku, float complex* alpha, float complex* a, int32_t* lda, float complex* x, int32_t* incx, float complex* beta, float complex* y, int32_t* incy);
void cgbmv64_(char* trans, int64_t* m, int64_t* n, int64_t* kl, int64_t* ku, float complex* alpha, float complex* a, int64_t* lda, float complex* x, int64_t* incx, float complex* beta, float complex* y, int64_t* incy);

void cgemm_(char* transa, char* transb, blasint* m, blasint* n, blasint* k, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* beta, float complex* c, blasint* ldc);
void cgemm32_(char* transa, char* transb, int32_t* m, int32_t* n, int32_t* k, float complex* alpha, float complex* a, int32_t* lda, float complex* b, int32_t* ldb, float complex* beta, float complex* c, int32_t* ldc);
void cgemm64_(char* transa, char* transb, int64_t* m, int64_t* n, int64_t* k, float complex* alpha, float complex* a, int64_t* lda, float complex* b, int64_t* ldb, float complex* beta, float complex* c, int64_t* ldc);

void cgemv_(char* trans, blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* x, blasint* incx, float complex* beta, float complex* y, blasint* incy);
void cgemv32_(char* trans, int32_t* m, int32_t* n, float complex* alpha, float complex* a, int32_t* lda, float complex* x, int32_t* incx, float complex* beta, float complex* y, int32_t* incy);
void cgemv64_(char* trans, int64_t* m, int64_t* n, float complex* alpha, float complex* a, int64_t* lda, float complex* x, int64_t* incx, float complex* beta, float complex* y, int64_t* incy);

void cgerc_(blasint* m, blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* y, blasint* incy, float complex* a, blasint* lda);
void cgerc32_(int32_t* m, int32_t* n, float complex* alpha, float complex* x, int32_t* incx, float complex* y, int32_t* incy, float complex* a, int32_t* lda);
void cgerc64_(int64_t* m, int64_t* n, float complex* alpha, float complex* x, int64_t* incx, float complex* y, int64_t* incy, float complex* a, int64_t* lda);

void cgeru_(blasint* m, blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* y, blasint* incy, float complex* a, blasint* lda);
void cgeru32_(int32_t* m, int32_t* n, float complex* alpha, float complex* x, int32_t* incx, float complex* y, int32_t* incy, float complex* a, int32_t* lda);
void cgeru64_(int64_t* m, int64_t* n, float complex* alpha, float complex* x, int64_t* incx, float complex* y, int64_t* incy, float complex* a, int64_t* lda);

void chbmv_(char* uplo, blasint* n, blasint* k, float complex* alpha, float complex* a, blasint* lda, float complex* x, blasint* incx, float complex* beta, float complex* y, blasint* incy);
void chbmv32_(char* uplo, int32_t* n, int32_t* k, float complex* alpha, float complex* a, int32_t* lda, float complex* x, int32_t* incx, float complex* beta, float complex* y, int32_t* incy);
void chbmv64_(char* uplo, int64_t* n, int64_t* k, float complex* alpha, float complex* a, int64_t* lda, float complex* x, int64_t* incx, float complex* beta, float complex* y, int64_t* incy);

void chemm_(char* side, char* uplo, blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* beta, float complex* c, blasint* ldc);
void chemm32_(char* side, char* uplo, int32_t* m, int32_t* n, float complex* alpha, float complex* a, int32_t* lda, float complex* b, int32_t* ldb, float complex* beta, float complex* c, int32_t* ldc);
void chemm64_(char* side, char* uplo, int64_t* m, int64_t* n, float complex* alpha, float complex* a, int64_t* lda, float complex* b, int64_t* ldb, float complex* beta, float complex* c, int64_t* ldc);

void chemv_(char* uplo, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* x, blasint* incx, float complex* beta, float complex* y, blasint* incy);
void chemv32_(char* uplo, int32_t* n, float complex* alpha, float complex* a, int32_t* lda, float complex* x, int32_t* incx, float complex* beta, float complex* y, int32_t* incy);
void chemv64_(char* uplo, int64_t* n, float complex* alpha, float complex* a, int64_t* lda, float complex* x, int64_t* incx, float complex* beta, float complex* y, int64_t* incy);

void cher_(char* uplo, blasint* n, float* alpha, float complex* x, blasint* incx, float complex* a, blasint* lda);
void cher32_(char* uplo, int32_t* n, float* alpha, float complex* x, int32_t* incx, float complex* a, int32_t* lda);
void cher64_(char* uplo, int64_t* n, float* alpha, float complex* x, int64_t* incx, float complex* a, int64_t* lda);

void cher2_(char* uplo, blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* y, blasint* incy, float complex* a, blasint* lda);
void cher232_(char* uplo, int32_t* n, float complex* alpha, float complex* x, int32_t* incx, float complex* y, int32_t* incy, float complex* a, int32_t* lda);
void cher264_(char* uplo, int64_t* n, float complex* alpha, float complex* x, int64_t* incx, float complex* y, int64_t* incy, float complex* a, int64_t* lda);

void cher2k_(char* uplo, char* trans, blasint* n, blasint* k, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* beta, float complex* c, blasint* ldc);
void cher2k32_(char* uplo, char* trans, int32_t* n, int32_t* k, float complex* alpha, float complex* a, int32_t* lda, float complex* b, int32_t* ldb, float* beta, float complex* c, int32_t* ldc);
void cher2k64_(char* uplo, char* trans, int64_t* n, int64_t* k, float complex* alpha, float complex* a, int64_t* lda, float complex* b, int64_t* ldb, float* beta, float complex* c, int64_t* ldc);

void cherk_(char* uplo, char* trans, blasint* n, blasint* k, float* alpha, float complex* a, blasint* lda, float* beta, float complex* c, blasint* ldc);
void cherk32_(char* uplo, char* trans, int32_t* n, int32_t* k, float* alpha, float complex* a, int32_t* lda, float* beta, float complex* c, int32_t* ldc);
void cherk64_(char* uplo, char* trans, int64_t* n, int64_t* k, float* alpha, float complex* a, int64_t* lda, float* beta, float complex* c, int64_t* ldc);

void chpmv_(char* uplo, blasint* n, float complex* alpha, float complex* ap, float complex* x, blasint* incx, float complex* beta, float complex* y, blasint* incy);
void chpmv32_(char* uplo, int32_t* n, float complex* alpha, float complex* ap, float complex* x, int32_t* incx, float complex* beta, float complex* y, int32_t* incy);
void chpmv64_(char* uplo, int64_t* n, float complex* alpha, float complex* ap, float complex* x, int64_t* incx, float complex* beta, float complex* y, int64_t* incy);

void chpr_(char* uplo, blasint* n, float* alpha, float complex* x, blasint* incx, float complex* ap);
void chpr32_(char* uplo, int32_t* n, float* alpha, float complex* x, int32_t* incx, float complex* ap);
void chpr64_(char* uplo, int64_t* n, float* alpha, float complex* x, int64_t* incx, float complex* ap);

void chpr2_(char* uplo, blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* y, blasint* incy, float complex* ap);
void chpr232_(char* uplo, int32_t* n, float complex* alpha, float complex* x, int32_t* incx, float complex* y, int32_t* incy, float complex* ap);
void chpr264_(char* uplo, int64_t* n, float complex* alpha, float complex* x, int64_t* incx, float complex* y, int64_t* incy, float complex* ap);

void crotg_(float complex* ca, float complex* cb, float* c, float complex* s);
void crotg32_(float complex* ca, float complex* cb, float* c, float complex* s);
void crotg64_(float complex* ca, float complex* cb, float* c, float complex* s);

void cscal_(blasint* n, float complex* ca, float complex* cx, blasint* incx);
void cscal32_(int32_t* n, float complex* ca, float complex* cx, int32_t* incx);
void cscal64_(int64_t* n, float complex* ca, float complex* cx, int64_t* incx);

void csrot_(blasint* n, float complex* cx, blasint* incx, float complex* cy, blasint* incy, float* c, float* s);
void csrot32_(int32_t* n, float complex* cx, int32_t* incx, float complex* cy, int32_t* incy, float* c, float* s);
void csrot64_(int64_t* n, float complex* cx, int64_t* incx, float complex* cy, int64_t* incy, float* c, float* s);

void csscal_(blasint* n, float* sa, float complex* cx, blasint* incx);
void csscal32_(int32_t* n, float* sa, float complex* cx, int32_t* incx);
void csscal64_(int64_t* n, float* sa, float complex* cx, int64_t* incx);

void cswap_(blasint* n, float complex* cx, blasint* incx, float complex* cy, blasint* incy);
void cswap32_(int32_t* n, float complex* cx, int32_t* incx, float complex* cy, int32_t* incy);
void cswap64_(int64_t* n, float complex* cx, int64_t* incx, float complex* cy, int64_t* incy);

void csymm_(char* side, char* uplo, blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* beta, float complex* c, blasint* ldc);
void csymm32_(char* side, char* uplo, int32_t* m, int32_t* n, float complex* alpha, float complex* a, int32_t* lda, float complex* b, int32_t* ldb, float complex* beta, float complex* c, int32_t* ldc);
void csymm64_(char* side, char* uplo, int64_t* m, int64_t* n, float complex* alpha, float complex* a, int64_t* lda, float complex* b, int64_t* ldb, float complex* beta, float complex* c, int64_t* ldc);

void csyr2k_(char* uplo, char* trans, blasint* n, blasint* k, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb, float complex* beta, float complex* c, blasint* ldc);
void csyr2k32_(char* uplo, char* trans, int32_t* n, int32_t* k, float complex* alpha, float complex* a, int32_t* lda, float complex* b, int32_t* ldb, float complex* beta, float complex* c, int32_t* ldc);
void csyr2k64_(char* uplo, char* trans, int64_t* n, int64_t* k, float complex* alpha, float complex* a, int64_t* lda, float complex* b, int64_t* ldb, float complex* beta, float complex* c, int64_t* ldc);

void csyrk_(char* uplo, char* trans, blasint* n, blasint* k, float complex* alpha, float complex* a, blasint* lda, float complex* beta, float complex* c, blasint* ldc);
void csyrk32_(char* uplo, char* trans, int32_t* n, int32_t* k, float complex* alpha, float complex* a, int32_t* lda, float complex* beta, float complex* c, int32_t* ldc);
void csyrk64_(char* uplo, char* trans, int64_t* n, int64_t* k, float complex* alpha, float complex* a, int64_t* lda, float complex* beta, float complex* c, int64_t* ldc);

void ctbmv_(char* uplo, char* trans, char* diag, blasint* n, blasint* k, float complex* a, blasint* lda, float complex* x, blasint* incx);
void ctbmv32_(char* uplo, char* trans, char* diag, int32_t* n, int32_t* k, float complex* a, int32_t* lda, float complex* x, int32_t* incx);
void ctbmv64_(char* uplo, char* trans, char* diag, int64_t* n, int64_t* k, float complex* a, int64_t* lda, float complex* x, int64_t* incx);

void ctbsv_(char* uplo, char* trans, char* diag, blasint* n, blasint* k, float complex* a, blasint* lda, float complex* x, blasint* incx);
void ctbsv32_(char* uplo, char* trans, char* diag, int32_t* n, int32_t* k, float complex* a, int32_t* lda, float complex* x, int32_t* incx);
void ctbsv64_(char* uplo, char* trans, char* diag, int64_t* n, int64_t* k, float complex* a, int64_t* lda, float complex* x, int64_t* incx);

void ctpmv_(char* uplo, char* trans, char* diag, blasint* n, float complex* ap, float complex* x, blasint* incx);
void ctpmv32_(char* uplo, char* trans, char* diag, int32_t* n, float complex* ap, float complex* x, int32_t* incx);
void ctpmv64_(char* uplo, char* trans, char* diag, int64_t* n, float complex* ap, float complex* x, int64_t* incx);

void ctpsv_(char* uplo, char* trans, char* diag, blasint* n, float complex* ap, float complex* x, blasint* incx);
void ctpsv32_(char* uplo, char* trans, char* diag, int32_t* n, float complex* ap, float complex* x, int32_t* incx);
void ctpsv64_(char* uplo, char* trans, char* diag, int64_t* n, float complex* ap, float complex* x, int64_t* incx);

void ctrmm_(char* side, char* uplo, char* transa, char* diag, blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb);
void ctrmm32_(char* side, char* uplo, char* transa, char* diag, int32_t* m, int32_t* n, float complex* alpha, float complex* a, int32_t* lda, float complex* b, int32_t* ldb);
void ctrmm64_(char* side, char* uplo, char* transa, char* diag, int64_t* m, int64_t* n, float complex* alpha, float complex* a, int64_t* lda, float complex* b, int64_t* ldb);

void ctrmv_(char* uplo, char* trans, char* diag, blasint* n, float complex* a, blasint* lda, float complex* x, blasint* incx);
void ctrmv32_(char* uplo, char* trans, char* diag, int32_t* n, float complex* a, int32_t* lda, float complex* x, int32_t* incx);
void ctrmv64_(char* uplo, char* trans, char* diag, int64_t* n, float complex* a, int64_t* lda, float complex* x, int64_t* incx);

void ctrsm_(char* side, char* uplo, char* transa, char* diag, blasint* m, blasint* n, float complex* alpha, float complex* a, blasint* lda, float complex* b, blasint* ldb);
void ctrsm32_(char* side, char* uplo, char* transa, char* diag, int32_t* m, int32_t* n, float complex* alpha, float complex* a, int32_t* lda, float complex* b, int32_t* ldb);
void ctrsm64_(char* side, char* uplo, char* transa, char* diag, int64_t* m, int64_t* n, float complex* alpha, float complex* a, int64_t* lda, float complex* b, int64_t* ldb);

void ctrsv_(char* uplo, char* trans, char* diag, blasint* n, float complex* a, blasint* lda, float complex* x, blasint* incx);
void ctrsv32_(char* uplo, char* trans, char* diag, int32_t* n, float complex* a, int32_t* lda, float complex* x, int32_t* incx);
void ctrsv64_(char* uplo, char* trans, char* diag, int64_t* n, float complex* a, int64_t* lda, float complex* x, int64_t* incx);

double dasum_(blasint* n, double* dx, blasint* incx);
double dasum32_(int32_t* n, double* dx, int32_t* incx);
double dasum64_(int64_t* n, double* dx, int64_t* incx);

void daxpy_(blasint* n, double* da, double* dx, blasint* incx, double* dy, blasint* incy);
void daxpy32_(int32_t* n, double* da, double* dx, int32_t* incx, double* dy, int32_t* incy);
void daxpy64_(int64_t* n, double* da, double* dx, int64_t* incx, double* dy, int64_t* incy);

void dcopy_(blasint* n, double* dx, blasint* incx, double* dy, blasint* incy);
void dcopy32_(int32_t* n, double* dx, int32_t* incx, double* dy, int32_t* incy);
void dcopy64_(int64_t* n, double* dx, int64_t* incx, double* dy, int64_t* incy);

double ddot_(blasint* n, double* dx, blasint* incx, double* dy, blasint* incy);
double ddot32_(int32_t* n, double* dx, int32_t* incx, double* dy, int32_t* incy);
double ddot64_(int64_t* n, double* dx, int64_t* incx, double* dy, int64_t* incy);

void dgbmv_(char* trans, blasint* m, blasint* n, blasint* kl, blasint* ku, double* alpha, double* a, blasint* lda, double* x, blasint* incx, double* beta, double* y, blasint* incy);
void dgbmv32_(char* trans, int32_t* m, int32_t* n, int32_t* kl, int32_t* ku, double* alpha, double* a, int32_t* lda, double* x, int32_t* incx, double* beta, double* y, int32_t* incy);
void dgbmv64_(char* trans, int64_t* m, int64_t* n, int64_t* kl, int64_t* ku, double* alpha, double* a, int64_t* lda, double* x, int64_t* incx, double* beta, double* y, int64_t* incy);

void dgemm_(char* transa, char* transb, blasint* m, blasint* n, blasint* k, double* alpha, double* a, blasint* lda, double* b, blasint* ldb, double* beta, double* c, blasint* ldc);
void dgemm32_(char* transa, char* transb, int32_t* m, int32_t* n, int32_t* k, double* alpha, double* a, int32_t* lda, double* b, int32_t* ldb, double* beta, double* c, int32_t* ldc);
void dgemm64_(char* transa, char* transb, int64_t* m, int64_t* n, int64_t* k, double* alpha, double* a, int64_t* lda, double* b, int64_t* ldb, double* beta, double* c, int64_t* ldc);

void dgemv_(char* trans, blasint* m, blasint* n, double* alpha, double* a, blasint* lda, double* x, blasint* incx, double* beta, double* y, blasint* incy);
void dgemv32_(char* trans, int32_t* m, int32_t* n, double* alpha, double* a, int32_t* lda, double* x, int32_t* incx, double* beta, double* y, int32_t* incy);
void dgemv64_(char* trans, int64_t* m, int64_t* n, double* alpha, double* a, int64_t* lda, double* x, int64_t* incx, double* beta, double* y, int64_t* incy);

void dger_(blasint* m, blasint* n, double* alpha, double* x, blasint* incx, double* y, blasint* incy, double* a, blasint* lda);
void dger32_(int32_t* m, int32_t* n, double* alpha, double* x, int32_t* incx, double* y, int32_t* incy, double* a, int32_t* lda);
void dger64_(int64_t* m, int64_t* n, double* alpha, double* x, int64_t* incx, double* y, int64_t* incy, double* a, int64_t* lda);

double dnrm2_(blasint* n, double* x, blasint* incx);
double dnrm232_(int32_t* n, double* x, int32_t* incx);
double dnrm264_(int64_t* n, double* x, int64_t* incx);

void drot_(blasint* n, double* dx, blasint* incx, double* dy, blasint* incy, double* c, double* s);
void drot32_(int32_t* n, double* dx, int32_t* incx, double* dy, int32_t* incy, double* c, double* s);
void drot64_(int64_t* n, double* dx, int64_t* incx, double* dy, int64_t* incy, double* c, double* s);

void drotg_(double* da, double* db, double* c, double* s);
void drotg32_(double* da, double* db, double* c, double* s);
void drotg64_(double* da, double* db, double* c, double* s);

void drotm_(blasint* n, double* dx, blasint* incx, double* dy, blasint* incy, double* dparam);
void drotm32_(int32_t* n, double* dx, int32_t* incx, double* dy, int32_t* incy, double* dparam);
void drotm64_(int64_t* n, double* dx, int64_t* incx, double* dy, int64_t* incy, double* dparam);

void drotmg_(double* dd1, double* dd2, double* dx1, double* dy1, double* dparam);
void drotmg32_(double* dd1, double* dd2, double* dx1, double* dy1, double* dparam);
void drotmg64_(double* dd1, double* dd2, double* dx1, double* dy1, double* dparam);

void dsbmv_(char* uplo, blasint* n, blasint* k, double* alpha, double* a, blasint* lda, double* x, blasint* incx, double* beta, double* y, blasint* incy);
void dsbmv32_(char* uplo, int32_t* n, int32_t* k, double* alpha, double* a, int32_t* lda, double* x, int32_t* incx, double* beta, double* y, int32_t* incy);
void dsbmv64_(char* uplo, int64_t* n, int64_t* k, double* alpha, double* a, int64_t* lda, double* x, int64_t* incx, double* beta, double* y, int64_t* incy);

void dscal_(blasint* n, double* da, double* dx, blasint* incx);
void dscal32_(int32_t* n, double* da, double* dx, int32_t* incx);
void dscal64_(int64_t* n, double* da, double* dx, int64_t* incx);

double dsdot_(blasint* n, float* sx, blasint* incx, float* sy, blasint* incy);
double dsdot32_(int32_t* n, float* sx, int32_t* incx, float* sy, int32_t* incy);
double dsdot64_(int64_t* n, float* sx, int64_t* incx, float* sy, int64_t* incy);

void dspmv_(char* uplo, blasint* n, double* alpha, double* ap, double* x, blasint* incx, double* beta, double* y, blasint* incy);
void dspmv32_(char* uplo, int32_t* n, double* alpha, double* ap, double* x, int32_t* incx, double* beta, double* y, int32_t* incy);
void dspmv64_(char* uplo, int64_t* n, double* alpha, double* ap, double* x, int64_t* incx, double* beta, double* y, int64_t* incy);

void dspr_(char* uplo, blasint* n, double* alpha, double* x, blasint* incx, double* ap);
void dspr32_(char* uplo, int32_t* n, double* alpha, double* x, int32_t* incx, double* ap);
void dspr64_(char* uplo, int64_t* n, double* alpha, double* x, int64_t* incx, double* ap);

void dspr2_(char* uplo, blasint* n, double* alpha, double* x, blasint* incx, double* y, blasint* incy, double* ap);
void dspr232_(char* uplo, int32_t* n, double* alpha, double* x, int32_t* incx, double* y, int32_t* incy, double* ap);
void dspr264_(char* uplo, int64_t* n, double* alpha, double* x, int64_t* incx, double* y, int64_t* incy, double* ap);

void dswap_(blasint* n, double* dx, blasint* incx, double* dy, blasint* incy);
void dswap32_(int32_t* n, double* dx, int32_t* incx, double* dy, int32_t* incy);
void dswap64_(int64_t* n, double* dx, int64_t* incx, double* dy, int64_t* incy);

void dsymm_(char* side, char* uplo, blasint* m, blasint* n, double* alpha, double* a, blasint* lda, double* b, blasint* ldb, double* beta, double* c, blasint* ldc);
void dsymm32_(char* side, char* uplo, int32_t* m, int32_t* n, double* alpha, double* a, int32_t* lda, double* b, int32_t* ldb, double* beta, double* c, int32_t* ldc);
void dsymm64_(char* side, char* uplo, int64_t* m, int64_t* n, double* alpha, double* a, int64_t* lda, double* b, int64_t* ldb, double* beta, double* c, int64_t* ldc);

void dsymv_(char* uplo, blasint* n, double* alpha, double* a, blasint* lda, double* x, blasint* incx, double* beta, double* y, blasint* incy);
void dsymv32_(char* uplo, int32_t* n, double* alpha, double* a, int32_t* lda, double* x, int32_t* incx, double* beta, double* y, int32_t* incy);
void dsymv64_(char* uplo, int64_t* n, double* alpha, double* a, int64_t* lda, double* x, int64_t* incx, double* beta, double* y, int64_t* incy);

void dsyr_(char* uplo, blasint* n, double* alpha, double* x, blasint* incx, double* a, blasint* lda);
void dsyr32_(char* uplo, int32_t* n, double* alpha, double* x, int32_t* incx, double* a, int32_t* lda);
void dsyr64_(char* uplo, int64_t* n, double* alpha, double* x, int64_t* incx, double* a, int64_t* lda);

void dsyr2_(char* uplo, blasint* n, double* alpha, double* x, blasint* incx, double* y, blasint* incy, double* a, blasint* lda);
void dsyr232_(char* uplo, int32_t* n, double* alpha, double* x, int32_t* incx, double* y, int32_t* incy, double* a, int32_t* lda);
void dsyr264_(char* uplo, int64_t* n, double* alpha, double* x, int64_t* incx, double* y, int64_t* incy, double* a, int64_t* lda);

void dsyr2k_(char* uplo, char* trans, blasint* n, blasint* k, double* alpha, double* a, blasint* lda, double* b, blasint* ldb, double* beta, double* c, blasint* ldc);
void dsyr2k32_(char* uplo, char* trans, int32_t* n, int32_t* k, double* alpha, double* a, int32_t* lda, double* b, int32_t* ldb, double* beta, double* c, int32_t* ldc);
void dsyr2k64_(char* uplo, char* trans, int64_t* n, int64_t* k, double* alpha, double* a, int64_t* lda, double* b, int64_t* ldb, double* beta, double* c, int64_t* ldc);

void dsyrk_(char* uplo, char* trans, blasint* n, blasint* k, double* alpha, double* a, blasint* lda, double* beta, double* c, blasint* ldc);
void dsyrk32_(char* uplo, char* trans, int32_t* n, int32_t* k, double* alpha, double* a, int32_t* lda, double* beta, double* c, int32_t* ldc);
void dsyrk64_(char* uplo, char* trans, int64_t* n, int64_t* k, double* alpha, double* a, int64_t* lda, double* beta, double* c, int64_t* ldc);

void dtbmv_(char* uplo, char* trans, char* diag, blasint* n, blasint* k, double* a, blasint* lda, double* x, blasint* incx);
void dtbmv32_(char* uplo, char* trans, char* diag, int32_t* n, int32_t* k, double* a, int32_t* lda, double* x, int32_t* incx);
void dtbmv64_(char* uplo, char* trans, char* diag, int64_t* n, int64_t* k, double* a, int64_t* lda, double* x, int64_t* incx);

void dtbsv_(char* uplo, char* trans, char* diag, blasint* n, blasint* k, double* a, blasint* lda, double* x, blasint* incx);
void dtbsv32_(char* uplo, char* trans, char* diag, int32_t* n, int32_t* k, double* a, int32_t* lda, double* x, int32_t* incx);
void dtbsv64_(char* uplo, char* trans, char* diag, int64_t* n, int64_t* k, double* a, int64_t* lda, double* x, int64_t* incx);

void dtpmv_(char* uplo, char* trans, char* diag, blasint* n, double* ap, double* x, blasint* incx);
void dtpmv32_(char* uplo, char* trans, char* diag, int32_t* n, double* ap, double* x, int32_t* incx);
void dtpmv64_(char* uplo, char* trans, char* diag, int64_t* n, double* ap, double* x, int64_t* incx);

void dtpsv_(char* uplo, char* trans, char* diag, blasint* n, double* ap, double* x, blasint* incx);
void dtpsv32_(char* uplo, char* trans, char* diag, int32_t* n, double* ap, double* x, int32_t* incx);
void dtpsv64_(char* uplo, char* trans, char* diag, int64_t* n, double* ap, double* x, int64_t* incx);

void dtrmm_(char* side, char* uplo, char* transa, char* diag, blasint* m, blasint* n, double* alpha, double* a, blasint* lda, double* b, blasint* ldb);
void dtrmm32_(char* side, char* uplo, char* transa, char* diag, int32_t* m, int32_t* n, double* alpha, double* a, int32_t* lda, double* b, int32_t* ldb);
void dtrmm64_(char* side, char* uplo, char* transa, char* diag, int64_t* m, int64_t* n, double* alpha, double* a, int64_t* lda, double* b, int64_t* ldb);

void dtrmv_(char* uplo, char* trans, char* diag, blasint* n, double* a, blasint* lda, double* x, blasint* incx);
void dtrmv32_(char* uplo, char* trans, char* diag, int32_t* n, double* a, int32_t* lda, double* x, int32_t* incx);
void dtrmv64_(char* uplo, char* trans, char* diag, int64_t* n, double* a, int64_t* lda, double* x, int64_t* incx);

void dtrsm_(char* side, char* uplo, char* transa, char* diag, blasint* m, blasint* n, double* alpha, double* a, blasint* lda, double* b, blasint* ldb);
void dtrsm32_(char* side, char* uplo, char* transa, char* diag, int32_t* m, int32_t* n, double* alpha, double* a, int32_t* lda, double* b, int32_t* ldb);
void dtrsm64_(char* side, char* uplo, char* transa, char* diag, int64_t* m, int64_t* n, double* alpha, double* a, int64_t* lda, double* b, int64_t* ldb);

void dtrsv_(char* uplo, char* trans, char* diag, blasint* n, double* a, blasint* lda, double* x, blasint* incx);
void dtrsv32_(char* uplo, char* trans, char* diag, int32_t* n, double* a, int32_t* lda, double* x, int32_t* incx);
void dtrsv64_(char* uplo, char* trans, char* diag, int64_t* n, double* a, int64_t* lda, double* x, int64_t* incx);

double dzasum_(blasint* n, double complex* zx, blasint* incx);
double dzasum32_(int32_t* n, double complex* zx, int32_t* incx);
double dzasum64_(int64_t* n, double complex* zx, int64_t* incx);

double dznrm2_(blasint* n, double complex* x, blasint* incx);
double dznrm232_(int32_t* n, double complex* x, int32_t* incx);
double dznrm264_(int64_t* n, double complex* x, int64_t* incx);

int icamax_(blasint* n, float complex* cx, blasint* incx);
int icamax32_(int32_t* n, float complex* cx, int32_t* incx);
int icamax64_(int64_t* n, float complex* cx, int64_t* incx);

int idamax_(blasint* n, double* dx, blasint* incx);
int idamax32_(int32_t* n, double* dx, int32_t* incx);
int idamax64_(int64_t* n, double* dx, int64_t* incx);

int isamax_(blasint* n, float* sx, blasint* incx);
int isamax32_(int32_t* n, float* sx, int32_t* incx);
int isamax64_(int64_t* n, float* sx, int64_t* incx);

int izamax_(blasint* n, double complex* zx, blasint* incx);
int izamax32_(int32_t* n, double complex* zx, int32_t* incx);
int izamax64_(int64_t* n, double complex* zx, int64_t* incx);

float sasum_(blasint* n, float* sx, blasint* incx);
float sasum32_(int32_t* n, float* sx, int32_t* incx);
float sasum64_(int64_t* n, float* sx, int64_t* incx);

void saxpy_(blasint* n, float* sa, float* sx, blasint* incx, float* sy, blasint* incy);
void saxpy32_(int32_t* n, float* sa, float* sx, int32_t* incx, float* sy, int32_t* incy);
void saxpy64_(int64_t* n, float* sa, float* sx, int64_t* incx, float* sy, int64_t* incy);

float scasum_(blasint* n, float complex* cx, blasint* incx);
float scasum32_(int32_t* n, float complex* cx, int32_t* incx);
float scasum64_(int64_t* n, float complex* cx, int64_t* incx);

float scnrm2_(blasint* n, float complex* x, blasint* incx);
float scnrm232_(int32_t* n, float complex* x, int32_t* incx);
float scnrm264_(int64_t* n, float complex* x, int64_t* incx);

void scopy_(blasint* n, float* sx, blasint* incx, float* sy, blasint* incy);
void scopy32_(int32_t* n, float* sx, int32_t* incx, float* sy, int32_t* incy);
void scopy64_(int64_t* n, float* sx, int64_t* incx, float* sy, int64_t* incy);

float sdot_(blasint* n, float* sx, blasint* incx, float* sy, blasint* incy);
float sdot32_(int32_t* n, float* sx, int32_t* incx, float* sy, int32_t* incy);
float sdot64_(int64_t* n, float* sx, int64_t* incx, float* sy, int64_t* incy);

float sdsdot_(blasint* n, float* sb, float* sx, blasint* incx, float* sy, blasint* incy);
float sdsdot32_(int32_t* n, float* sb, float* sx, int32_t* incx, float* sy, int32_t* incy);
float sdsdot64_(int64_t* n, float* sb, float* sx, int64_t* incx, float* sy, int64_t* incy);

void sgbmv_(char* trans, blasint* m, blasint* n, blasint* kl, blasint* ku, float* alpha, float* a, blasint* lda, float* x, blasint* incx, float* beta, float* y, blasint* incy);
void sgbmv32_(char* trans, int32_t* m, int32_t* n, int32_t* kl, int32_t* ku, float* alpha, float* a, int32_t* lda, float* x, int32_t* incx, float* beta, float* y, int32_t* incy);
void sgbmv64_(char* trans, int64_t* m, int64_t* n, int64_t* kl, int64_t* ku, float* alpha, float* a, int64_t* lda, float* x, int64_t* incx, float* beta, float* y, int64_t* incy);

void sgemm_(char* transa, char* transb, blasint* m, blasint* n, blasint* k, float* alpha, float* a, blasint* lda, float* b, blasint* ldb, float* beta, float* c, blasint* ldc);
void sgemm32_(char* transa, char* transb, int32_t* m, int32_t* n, int32_t* k, float* alpha, float* a, int32_t* lda, float* b, int32_t* ldb, float* beta, float* c, int32_t* ldc);
void sgemm64_(char* transa, char* transb, int64_t* m, int64_t* n, int64_t* k, float* alpha, float* a, int64_t* lda, float* b, int64_t* ldb, float* beta, float* c, int64_t* ldc);

void sgemv_(char* trans, blasint* m, blasint* n, float* alpha, float* a, blasint* lda, float* x, blasint* incx, float* beta, float* y, blasint* incy);
void sgemv32_(char* trans, int32_t* m, int32_t* n, float* alpha, float* a, int32_t* lda, float* x, int32_t* incx, float* beta, float* y, int32_t* incy);
void sgemv64_(char* trans, int64_t* m, int64_t* n, float* alpha, float* a, int64_t* lda, float* x, int64_t* incx, float* beta, float* y, int64_t* incy);

void sger_(blasint* m, blasint* n, float* alpha, float* x, blasint* incx, float* y, blasint* incy, float* a, blasint* lda);
void sger32_(int32_t* m, int32_t* n, float* alpha, float* x, int32_t* incx, float* y, int32_t* incy, float* a, int32_t* lda);
void sger64_(int64_t* m, int64_t* n, float* alpha, float* x, int64_t* incx, float* y, int64_t* incy, float* a, int64_t* lda);

float snrm2_(blasint* n, float* x, blasint* incx);
float snrm232_(int32_t* n, float* x, int32_t* incx);
float snrm264_(int64_t* n, float* x, int64_t* incx);

void srot_(blasint* n, float* sx, blasint* incx, float* sy, blasint* incy, float* c, float* s);
void srot32_(int32_t* n, float* sx, int32_t* incx, float* sy, int32_t* incy, float* c, float* s);
void srot64_(int64_t* n, float* sx, int64_t* incx, float* sy, int64_t* incy, float* c, float* s);

void srotg_(float* sa, float* sb, float* c, float* s);
void srotg32_(float* sa, float* sb, float* c, float* s);
void srotg64_(float* sa, float* sb, float* c, float* s);

void srotm_(blasint* n, float* sx, blasint* incx, float* sy, blasint* incy, float* sparam);
void srotm32_(int32_t* n, float* sx, int32_t* incx, float* sy, int32_t* incy, float* sparam);
void srotm64_(int64_t* n, float* sx, int64_t* incx, float* sy, int64_t* incy, float* sparam);

void srotmg_(float* sd1, float* sd2, float* sx1, float* sy1, float* sparam);
void srotmg32_(float* sd1, float* sd2, float* sx1, float* sy1, float* sparam);
void srotmg64_(float* sd1, float* sd2, float* sx1, float* sy1, float* sparam);

void ssbmv_(char* uplo, blasint* n, blasint* k, float* alpha, float* a, blasint* lda, float* x, blasint* incx, float* beta, float* y, blasint* incy);
void ssbmv32_(char* uplo, int32_t* n, int32_t* k, float* alpha, float* a, int32_t* lda, float* x, int32_t* incx, float* beta, float* y, int32_t* incy);
void ssbmv64_(char* uplo, int64_t* n, int64_t* k, float* alpha, float* a, int64_t* lda, float* x, int64_t* incx, float* beta, float* y, int64_t* incy);

void sscal_(blasint* n, float* sa, float* sx, blasint* incx);
void sscal32_(int32_t* n, float* sa, float* sx, int32_t* incx);
void sscal64_(int64_t* n, float* sa, float* sx, int64_t* incx);

void sspmv_(char* uplo, blasint* n, float* alpha, float* ap, float* x, blasint* incx, float* beta, float* y, blasint* incy);
void sspmv32_(char* uplo, int32_t* n, float* alpha, float* ap, float* x, int32_t* incx, float* beta, float* y, int32_t* incy);
void sspmv64_(char* uplo, int64_t* n, float* alpha, float* ap, float* x, int64_t* incx, float* beta, float* y, int64_t* incy);

void sspr_(char* uplo, blasint* n, float* alpha, float* x, blasint* incx, float* ap);
void sspr32_(char* uplo, int32_t* n, float* alpha, float* x, int32_t* incx, float* ap);
void sspr64_(char* uplo, int64_t* n, float* alpha, float* x, int64_t* incx, float* ap);

void sspr2_(char* uplo, blasint* n, float* alpha, float* x, blasint* incx, float* y, blasint* incy, float* ap);
void sspr232_(char* uplo, int32_t* n, float* alpha, float* x, int32_t* incx, float* y, int32_t* incy, float* ap);
void sspr264_(char* uplo, int64_t* n, float* alpha, float* x, int64_t* incx, float* y, int64_t* incy, float* ap);

void sswap_(blasint* n, float* sx, blasint* incx, float* sy, blasint* incy);
void sswap32_(int32_t* n, float* sx, int32_t* incx, float* sy, int32_t* incy);
void sswap64_(int64_t* n, float* sx, int64_t* incx, float* sy, int64_t* incy);

void ssymm_(char* side, char* uplo, blasint* m, blasint* n, float* alpha, float* a, blasint* lda, float* b, blasint* ldb, float* beta, float* c, blasint* ldc);
void ssymm32_(char* side, char* uplo, int32_t* m, int32_t* n, float* alpha, float* a, int32_t* lda, float* b, int32_t* ldb, float* beta, float* c, int32_t* ldc);
void ssymm64_(char* side, char* uplo, int64_t* m, int64_t* n, float* alpha, float* a, int64_t* lda, float* b, int64_t* ldb, float* beta, float* c, int64_t* ldc);

void ssymv_(char* uplo, blasint* n, float* alpha, float* a, blasint* lda, float* x, blasint* incx, float* beta, float* y, blasint* incy);
void ssymv32_(char* uplo, int32_t* n, float* alpha, float* a, int32_t* lda, float* x, int32_t* incx, float* beta, float* y, int32_t* incy);
void ssymv64_(char* uplo, int64_t* n, float* alpha, float* a, int64_t* lda, float* x, int64_t* incx, float* beta, float* y, int64_t* incy);

void ssyr_(char* uplo, blasint* n, float* alpha, float* x, blasint* incx, float* a, blasint* lda);
void ssyr32_(char* uplo, int32_t* n, float* alpha, float* x, int32_t* incx, float* a, int32_t* lda);
void ssyr64_(char* uplo, int64_t* n, float* alpha, float* x, int64_t* incx, float* a, int64_t* lda);

void ssyr2_(char* uplo, blasint* n, float* alpha, float* x, blasint* incx, float* y, blasint* incy, float* a, blasint* lda);
void ssyr232_(char* uplo, int32_t* n, float* alpha, float* x, int32_t* incx, float* y, int32_t* incy, float* a, int32_t* lda);
void ssyr264_(char* uplo, int64_t* n, float* alpha, float* x, int64_t* incx, float* y, int64_t* incy, float* a, int64_t* lda);

void ssyr2k_(char* uplo, char* trans, blasint* n, blasint* k, float* alpha, float* a, blasint* lda, float* b, blasint* ldb, float* beta, float* c, blasint* ldc);
void ssyr2k32_(char* uplo, char* trans, int32_t* n, int32_t* k, float* alpha, float* a, int32_t* lda, float* b, int32_t* ldb, float* beta, float* c, int32_t* ldc);
void ssyr2k64_(char* uplo, char* trans, int64_t* n, int64_t* k, float* alpha, float* a, int64_t* lda, float* b, int64_t* ldb, float* beta, float* c, int64_t* ldc);

void ssyrk_(char* uplo, char* trans, blasint* n, blasint* k, float* alpha, float* a, blasint* lda, float* beta, float* c, blasint* ldc);
void ssyrk32_(char* uplo, char* trans, int32_t* n, int32_t* k, float* alpha, float* a, int32_t* lda, float* beta, float* c, int32_t* ldc);
void ssyrk64_(char* uplo, char* trans, int64_t* n, int64_t* k, float* alpha, float* a, int64_t* lda, float* beta, float* c, int64_t* ldc);

void stbmv_(char* uplo, char* trans, char* diag, blasint* n, blasint* k, float* a, blasint* lda, float* x, blasint* incx);
void stbmv32_(char* uplo, char* trans, char* diag, int32_t* n, int32_t* k, float* a, int32_t* lda, float* x, int32_t* incx);
void stbmv64_(char* uplo, char* trans, char* diag, int64_t* n, int64_t* k, float* a, int64_t* lda, float* x, int64_t* incx);

void stbsv_(char* uplo, char* trans, char* diag, blasint* n, blasint* k, float* a, blasint* lda, float* x, blasint* incx);
void stbsv32_(char* uplo, char* trans, char* diag, int32_t* n, int32_t* k, float* a, int32_t* lda, float* x, int32_t* incx);
void stbsv64_(char* uplo, char* trans, char* diag, int64_t* n, int64_t* k, float* a, int64_t* lda, float* x, int64_t* incx);

void stpmv_(char* uplo, char* trans, char* diag, blasint* n, float* ap, float* x, blasint* incx);
void stpmv32_(char* uplo, char* trans, char* diag, int32_t* n, float* ap, float* x, int32_t* incx);
void stpmv64_(char* uplo, char* trans, char* diag, int64_t* n, float* ap, float* x, int64_t* incx);

void stpsv_(char* uplo, char* trans, char* diag, blasint* n, float* ap, float* x, blasint* incx);
void stpsv32_(char* uplo, char* trans, char* diag, int32_t* n, float* ap, float* x, int32_t* incx);
void stpsv64_(char* uplo, char* trans, char* diag, int64_t* n, float* ap, float* x, int64_t* incx);

void strmm_(char* side, char* uplo, char* transa, char* diag, blasint* m, blasint* n, float* alpha, float* a, blasint* lda, float* b, blasint* ldb);
void strmm32_(char* side, char* uplo, char* transa, char* diag, int32_t* m, int32_t* n, float* alpha, float* a, int32_t* lda, float* b, int32_t* ldb);
void strmm64_(char* side, char* uplo, char* transa, char* diag, int64_t* m, int64_t* n, float* alpha, float* a, int64_t* lda, float* b, int64_t* ldb);

void strmv_(char* uplo, char* trans, char* diag, blasint* n, float* a, blasint* lda, float* x, blasint* incx);
void strmv32_(char* uplo, char* trans, char* diag, int32_t* n, float* a, int32_t* lda, float* x, int32_t* incx);
void strmv64_(char* uplo, char* trans, char* diag, int64_t* n, float* a, int64_t* lda, float* x, int64_t* incx);

void strsm_(char* side, char* uplo, char* transa, char* diag, blasint* m, blasint* n, float* alpha, float* a, blasint* lda, float* b, blasint* ldb);
void strsm32_(char* side, char* uplo, char* transa, char* diag, int32_t* m, int32_t* n, float* alpha, float* a, int32_t* lda, float* b, int32_t* ldb);
void strsm64_(char* side, char* uplo, char* transa, char* diag, int64_t* m, int64_t* n, float* alpha, float* a, int64_t* lda, float* b, int64_t* ldb);

void strsv_(char* uplo, char* trans, char* diag, blasint* n, float* a, blasint* lda, float* x, blasint* incx);
void strsv32_(char* uplo, char* trans, char* diag, int32_t* n, float* a, int32_t* lda, float* x, int32_t* incx);
void strsv64_(char* uplo, char* trans, char* diag, int64_t* n, float* a, int64_t* lda, float* x, int64_t* incx);

void zaxpy_(blasint* n, double complex* za, double complex* zx, blasint* incx, double complex* zy, blasint* incy);
void zaxpy32_(int32_t* n, double complex* za, double complex* zx, int32_t* incx, double complex* zy, int32_t* incy);
void zaxpy64_(int64_t* n, double complex* za, double complex* zx, int64_t* incx, double complex* zy, int64_t* incy);

void zcopy_(blasint* n, double complex* zx, blasint* incx, double complex* zy, blasint* incy);
void zcopy32_(int32_t* n, double complex* zx, int32_t* incx, double complex* zy, int32_t* incy);
void zcopy64_(int64_t* n, double complex* zx, int64_t* incx, double complex* zy, int64_t* incy);

void zdotc_( double complex* returnvalue, blasint* n, double complex* zx, blasint* incx, double complex* zy, blasint* incy);
void zdotc32_( double complex* returnvalue, int32_t* n, double complex* zx, int32_t* incx, double complex* zy, int32_t* incy);
void zdotc64_( double complex* returnvalue, int64_t* n, double complex* zx, int64_t* incx, double complex* zy, int64_t* incy);

void zdotu_( double complex* returnvalue, blasint* n, double complex* zx, blasint* incx, double complex* zy, blasint* incy);
void zdotu32_( double complex* returnvalue, int32_t* n, double complex* zx, int32_t* incx, double complex* zy, int32_t* incy);
void zdotu64_( double complex* returnvalue, int64_t* n, double complex* zx, int64_t* incx, double complex* zy, int64_t* incy);

void zdrot_(blasint* n, double complex* cx, blasint* incx, double complex* cy, blasint* incy, double* c, double* s);
void zdrot32_(int32_t* n, double complex* cx, int32_t* incx, double complex* cy, int32_t* incy, double* c, double* s);
void zdrot64_(int64_t* n, double complex* cx, int64_t* incx, double complex* cy, int64_t* incy, double* c, double* s);

void zdscal_(blasint* n, double* da, double complex* zx, blasint* incx);
void zdscal32_(int32_t* n, double* da, double complex* zx, int32_t* incx);
void zdscal64_(int64_t* n, double* da, double complex* zx, int64_t* incx);

void zgbmv_(char* trans, blasint* m, blasint* n, blasint* kl, blasint* ku, double complex* alpha, double complex* a, blasint* lda, double complex* x, blasint* incx, double complex* beta, double complex* y, blasint* incy);
void zgbmv32_(char* trans, int32_t* m, int32_t* n, int32_t* kl, int32_t* ku, double complex* alpha, double complex* a, int32_t* lda, double complex* x, int32_t* incx, double complex* beta, double complex* y, int32_t* incy);
void zgbmv64_(char* trans, int64_t* m, int64_t* n, int64_t* kl, int64_t* ku, double complex* alpha, double complex* a, int64_t* lda, double complex* x, int64_t* incx, double complex* beta, double complex* y, int64_t* incy);

void zgemm_(char* transa, char* transb, blasint* m, blasint* n, blasint* k, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* beta, double complex* c, blasint* ldc);
void zgemm32_(char* transa, char* transb, int32_t* m, int32_t* n, int32_t* k, double complex* alpha, double complex* a, int32_t* lda, double complex* b, int32_t* ldb, double complex* beta, double complex* c, int32_t* ldc);
void zgemm64_(char* transa, char* transb, int64_t* m, int64_t* n, int64_t* k, double complex* alpha, double complex* a, int64_t* lda, double complex* b, int64_t* ldb, double complex* beta, double complex* c, int64_t* ldc);

void zgemv_(char* trans, blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* x, blasint* incx, double complex* beta, double complex* y, blasint* incy);
void zgemv32_(char* trans, int32_t* m, int32_t* n, double complex* alpha, double complex* a, int32_t* lda, double complex* x, int32_t* incx, double complex* beta, double complex* y, int32_t* incy);
void zgemv64_(char* trans, int64_t* m, int64_t* n, double complex* alpha, double complex* a, int64_t* lda, double complex* x, int64_t* incx, double complex* beta, double complex* y, int64_t* incy);

void zgerc_(blasint* m, blasint* n, double complex* alpha, double complex* x, blasint* incx, double complex* y, blasint* incy, double complex* a, blasint* lda);
void zgerc32_(int32_t* m, int32_t* n, double complex* alpha, double complex* x, int32_t* incx, double complex* y, int32_t* incy, double complex* a, int32_t* lda);
void zgerc64_(int64_t* m, int64_t* n, double complex* alpha, double complex* x, int64_t* incx, double complex* y, int64_t* incy, double complex* a, int64_t* lda);

void zgeru_(blasint* m, blasint* n, double complex* alpha, double complex* x, blasint* incx, double complex* y, blasint* incy, double complex* a, blasint* lda);
void zgeru32_(int32_t* m, int32_t* n, double complex* alpha, double complex* x, int32_t* incx, double complex* y, int32_t* incy, double complex* a, int32_t* lda);
void zgeru64_(int64_t* m, int64_t* n, double complex* alpha, double complex* x, int64_t* incx, double complex* y, int64_t* incy, double complex* a, int64_t* lda);

void zhbmv_(char* uplo, blasint* n, blasint* k, double complex* alpha, double complex* a, blasint* lda, double complex* x, blasint* incx, double complex* beta, double complex* y, blasint* incy);
void zhbmv32_(char* uplo, int32_t* n, int32_t* k, double complex* alpha, double complex* a, int32_t* lda, double complex* x, int32_t* incx, double complex* beta, double complex* y, int32_t* incy);
void zhbmv64_(char* uplo, int64_t* n, int64_t* k, double complex* alpha, double complex* a, int64_t* lda, double complex* x, int64_t* incx, double complex* beta, double complex* y, int64_t* incy);

void zhemm_(char* side, char* uplo, blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* beta, double complex* c, blasint* ldc);
void zhemm32_(char* side, char* uplo, int32_t* m, int32_t* n, double complex* alpha, double complex* a, int32_t* lda, double complex* b, int32_t* ldb, double complex* beta, double complex* c, int32_t* ldc);
void zhemm64_(char* side, char* uplo, int64_t* m, int64_t* n, double complex* alpha, double complex* a, int64_t* lda, double complex* b, int64_t* ldb, double complex* beta, double complex* c, int64_t* ldc);

void zhemv_(char* uplo, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* x, blasint* incx, double complex* beta, double complex* y, blasint* incy);
void zhemv32_(char* uplo, int32_t* n, double complex* alpha, double complex* a, int32_t* lda, double complex* x, int32_t* incx, double complex* beta, double complex* y, int32_t* incy);
void zhemv64_(char* uplo, int64_t* n, double complex* alpha, double complex* a, int64_t* lda, double complex* x, int64_t* incx, double complex* beta, double complex* y, int64_t* incy);

void zher_(char* uplo, blasint* n, double* alpha, double complex* x, blasint* incx, double complex* a, blasint* lda);
void zher32_(char* uplo, int32_t* n, double* alpha, double complex* x, int32_t* incx, double complex* a, int32_t* lda);
void zher64_(char* uplo, int64_t* n, double* alpha, double complex* x, int64_t* incx, double complex* a, int64_t* lda);

void zher2_(char* uplo, blasint* n, double complex* alpha, double complex* x, blasint* incx, double complex* y, blasint* incy, double complex* a, blasint* lda);
void zher232_(char* uplo, int32_t* n, double complex* alpha, double complex* x, int32_t* incx, double complex* y, int32_t* incy, double complex* a, int32_t* lda);
void zher264_(char* uplo, int64_t* n, double complex* alpha, double complex* x, int64_t* incx, double complex* y, int64_t* incy, double complex* a, int64_t* lda);

void zher2k_(char* uplo, char* trans, blasint* n, blasint* k, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb, double* beta, double complex* c, blasint* ldc);
void zher2k32_(char* uplo, char* trans, int32_t* n, int32_t* k, double complex* alpha, double complex* a, int32_t* lda, double complex* b, int32_t* ldb, double* beta, double complex* c, int32_t* ldc);
void zher2k64_(char* uplo, char* trans, int64_t* n, int64_t* k, double complex* alpha, double complex* a, int64_t* lda, double complex* b, int64_t* ldb, double* beta, double complex* c, int64_t* ldc);

void zherk_(char* uplo, char* trans, blasint* n, blasint* k, double* alpha, double complex* a, blasint* lda, double* beta, double complex* c, blasint* ldc);
void zherk32_(char* uplo, char* trans, int32_t* n, int32_t* k, double* alpha, double complex* a, int32_t* lda, double* beta, double complex* c, int32_t* ldc);
void zherk64_(char* uplo, char* trans, int64_t* n, int64_t* k, double* alpha, double complex* a, int64_t* lda, double* beta, double complex* c, int64_t* ldc);

void zhpmv_(char* uplo, blasint* n, double complex* alpha, double complex* ap, double complex* x, blasint* incx, double complex* beta, double complex* y, blasint* incy);
void zhpmv32_(char* uplo, int32_t* n, double complex* alpha, double complex* ap, double complex* x, int32_t* incx, double complex* beta, double complex* y, int32_t* incy);
void zhpmv64_(char* uplo, int64_t* n, double complex* alpha, double complex* ap, double complex* x, int64_t* incx, double complex* beta, double complex* y, int64_t* incy);

void zhpr_(char* uplo, blasint* n, double* alpha, double complex* x, blasint* incx, double complex* ap);
void zhpr32_(char* uplo, int32_t* n, double* alpha, double complex* x, int32_t* incx, double complex* ap);
void zhpr64_(char* uplo, int64_t* n, double* alpha, double complex* x, int64_t* incx, double complex* ap);

void zhpr2_(char* uplo, blasint* n, double complex* alpha, double complex* x, blasint* incx, double complex* y, blasint* incy, double complex* ap);
void zhpr232_(char* uplo, int32_t* n, double complex* alpha, double complex* x, int32_t* incx, double complex* y, int32_t* incy, double complex* ap);
void zhpr264_(char* uplo, int64_t* n, double complex* alpha, double complex* x, int64_t* incx, double complex* y, int64_t* incy, double complex* ap);

void zrotg_(double complex* ca, double complex* cb, double* c, double complex* s);
void zrotg32_(double complex* ca, double complex* cb, double* c, double complex* s);
void zrotg64_(double complex* ca, double complex* cb, double* c, double complex* s);

void zscal_(blasint* n, double complex* za, double complex* zx, blasint* incx);
void zscal32_(int32_t* n, double complex* za, double complex* zx, int32_t* incx);
void zscal64_(int64_t* n, double complex* za, double complex* zx, int64_t* incx);

void zswap_(blasint* n, double complex* zx, blasint* incx, double complex* zy, blasint* incy);
void zswap32_(int32_t* n, double complex* zx, int32_t* incx, double complex* zy, int32_t* incy);
void zswap64_(int64_t* n, double complex* zx, int64_t* incx, double complex* zy, int64_t* incy);

void zsymm_(char* side, char* uplo, blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* beta, double complex* c, blasint* ldc);
void zsymm32_(char* side, char* uplo, int32_t* m, int32_t* n, double complex* alpha, double complex* a, int32_t* lda, double complex* b, int32_t* ldb, double complex* beta, double complex* c, int32_t* ldc);
void zsymm64_(char* side, char* uplo, int64_t* m, int64_t* n, double complex* alpha, double complex* a, int64_t* lda, double complex* b, int64_t* ldb, double complex* beta, double complex* c, int64_t* ldc);

void zsyr2k_(char* uplo, char* trans, blasint* n, blasint* k, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* beta, double complex* c, blasint* ldc);
void zsyr2k32_(char* uplo, char* trans, int32_t* n, int32_t* k, double complex* alpha, double complex* a, int32_t* lda, double complex* b, int32_t* ldb, double complex* beta, double complex* c, int32_t* ldc);
void zsyr2k64_(char* uplo, char* trans, int64_t* n, int64_t* k, double complex* alpha, double complex* a, int64_t* lda, double complex* b, int64_t* ldb, double complex* beta, double complex* c, int64_t* ldc);

void zsyrk_(char* uplo, char* trans, blasint* n, blasint* k, double complex* alpha, double complex* a, blasint* lda, double complex* beta, double complex* c, blasint* ldc);
void zsyrk32_(char* uplo, char* trans, int32_t* n, int32_t* k, double complex* alpha, double complex* a, int32_t* lda, double complex* beta, double complex* c, int32_t* ldc);
void zsyrk64_(char* uplo, char* trans, int64_t* n, int64_t* k, double complex* alpha, double complex* a, int64_t* lda, double complex* beta, double complex* c, int64_t* ldc);

void ztbmv_(char* uplo, char* trans, char* diag, blasint* n, blasint* k, double complex* a, blasint* lda, double complex* x, blasint* incx);
void ztbmv32_(char* uplo, char* trans, char* diag, int32_t* n, int32_t* k, double complex* a, int32_t* lda, double complex* x, int32_t* incx);
void ztbmv64_(char* uplo, char* trans, char* diag, int64_t* n, int64_t* k, double complex* a, int64_t* lda, double complex* x, int64_t* incx);

void ztbsv_(char* uplo, char* trans, char* diag, blasint* n, blasint* k, double complex* a, blasint* lda, double complex* x, blasint* incx);
void ztbsv32_(char* uplo, char* trans, char* diag, int32_t* n, int32_t* k, double complex* a, int32_t* lda, double complex* x, int32_t* incx);
void ztbsv64_(char* uplo, char* trans, char* diag, int64_t* n, int64_t* k, double complex* a, int64_t* lda, double complex* x, int64_t* incx);

void ztpmv_(char* uplo, char* trans, char* diag, blasint* n, double complex* ap, double complex* x, blasint* incx);
void ztpmv32_(char* uplo, char* trans, char* diag, int32_t* n, double complex* ap, double complex* x, int32_t* incx);
void ztpmv64_(char* uplo, char* trans, char* diag, int64_t* n, double complex* ap, double complex* x, int64_t* incx);

void ztpsv_(char* uplo, char* trans, char* diag, blasint* n, double complex* ap, double complex* x, blasint* incx);
void ztpsv32_(char* uplo, char* trans, char* diag, int32_t* n, double complex* ap, double complex* x, int32_t* incx);
void ztpsv64_(char* uplo, char* trans, char* diag, int64_t* n, double complex* ap, double complex* x, int64_t* incx);

void ztrmm_(char* side, char* uplo, char* transa, char* diag, blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb);
void ztrmm32_(char* side, char* uplo, char* transa, char* diag, int32_t* m, int32_t* n, double complex* alpha, double complex* a, int32_t* lda, double complex* b, int32_t* ldb);
void ztrmm64_(char* side, char* uplo, char* transa, char* diag, int64_t* m, int64_t* n, double complex* alpha, double complex* a, int64_t* lda, double complex* b, int64_t* ldb);

void ztrmv_(char* uplo, char* trans, char* diag, blasint* n, double complex* a, blasint* lda, double complex* x, blasint* incx);
void ztrmv32_(char* uplo, char* trans, char* diag, int32_t* n, double complex* a, int32_t* lda, double complex* x, int32_t* incx);
void ztrmv64_(char* uplo, char* trans, char* diag, int64_t* n, double complex* a, int64_t* lda, double complex* x, int64_t* incx);

void ztrsm_(char* side, char* uplo, char* transa, char* diag, blasint* m, blasint* n, double complex* alpha, double complex* a, blasint* lda, double complex* b, blasint* ldb);
void ztrsm32_(char* side, char* uplo, char* transa, char* diag, int32_t* m, int32_t* n, double complex* alpha, double complex* a, int32_t* lda, double complex* b, int32_t* ldb);
void ztrsm64_(char* side, char* uplo, char* transa, char* diag, int64_t* m, int64_t* n, double complex* alpha, double complex* a, int64_t* lda, double complex* b, int64_t* ldb);

void ztrsv_(char* uplo, char* trans, char* diag, blasint* n, double complex* a, blasint* lda, double complex* x, blasint* incx);
void ztrsv32_(char* uplo, char* trans, char* diag, int32_t* n, double complex* a, int32_t* lda, double complex* x, int32_t* incx);
void ztrsv64_(char* uplo, char* trans, char* diag, int64_t* n, double complex* a, int64_t* lda, double complex* x, int64_t* incx);


#ifdef __cplusplus
}
#endif
#endif
