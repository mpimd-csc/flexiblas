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

/* Prototypes for external procedures generated from cla_gbamv.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cla_gbamv_ (int *trans, int *m, int *n, int *kl, int *ku, float *alpha, __GFORTRAN_FLOAT_COMPLEX *ab, int *ldab, __GFORTRAN_FLOAT_COMPLEX *x, int *incx, float *beta, float *y, int *incy);

#ifdef __cplusplus
}
#endif
