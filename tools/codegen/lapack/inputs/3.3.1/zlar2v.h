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

/* Prototypes for external procedures generated from zlar2v.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlar2v_ (int *n, __GFORTRAN_DOUBLE_COMPLEX *x, __GFORTRAN_DOUBLE_COMPLEX *y, __GFORTRAN_DOUBLE_COMPLEX *z, int *incx, double *c, __GFORTRAN_DOUBLE_COMPLEX *s, int *incc);

#ifdef __cplusplus
}
#endif
