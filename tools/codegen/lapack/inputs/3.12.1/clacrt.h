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

/* Prototypes for external procedures generated from clacrt.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void clacrt_ (int *n, __GFORTRAN_FLOAT_COMPLEX *cx, int *incx, __GFORTRAN_FLOAT_COMPLEX *cy, int *incy, __GFORTRAN_FLOAT_COMPLEX *c, __GFORTRAN_FLOAT_COMPLEX *s);

#ifdef __cplusplus
}
#endif
