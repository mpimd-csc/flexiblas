#include <stddef.h>
#ifdef __cplusplus
#include <complex>
#define __GFORTRAN_FLOAT_COMPLEX std::complex<float>
#define __GFORTRAN_DOUBLE_COMPLEX std::complex<double>
#define __GFORTRAN_LONG_DOUBLE_COMPLEX std::complex<long double>
extern "C" {
#else
#define __GFORTRAN_FLOAT_COMPLEX float _Complex
#define __GFORTRAN_DOUBLE_COMPLEX double _Complex
#define __GFORTRAN_LONG_DOUBLE_COMPLEX long double _Complex
#endif

/* Prototypes for external procedures generated from ctrsv.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void ctrsv_ (char *uplo, char *trans, char *diag, int *n, __GFORTRAN_FLOAT_COMPLEX *a, int *lda, __GFORTRAN_FLOAT_COMPLEX *x, int *incx, size_t uplo_len, size_t trans_len, size_t diag_len);

#ifdef __cplusplus
}
#endif
