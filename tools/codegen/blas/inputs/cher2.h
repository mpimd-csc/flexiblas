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

/* Prototypes for external procedures generated from cher2.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cher2_ (char *uplo, int *n, __GFORTRAN_FLOAT_COMPLEX *alpha, __GFORTRAN_FLOAT_COMPLEX *x, int *incx, __GFORTRAN_FLOAT_COMPLEX *y, int *incy, __GFORTRAN_FLOAT_COMPLEX *a, int *lda, size_t uplo_len);

#ifdef __cplusplus
}
#endif
