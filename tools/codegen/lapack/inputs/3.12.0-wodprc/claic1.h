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

/* Prototypes for external procedures generated from claic1.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void claic1_ (int *job, int *j, __GFORTRAN_FLOAT_COMPLEX *x, float *sest, __GFORTRAN_FLOAT_COMPLEX *w, __GFORTRAN_FLOAT_COMPLEX *gamma, float *sestpr, __GFORTRAN_FLOAT_COMPLEX *s, __GFORTRAN_FLOAT_COMPLEX *c);

#ifdef __cplusplus
}
#endif
