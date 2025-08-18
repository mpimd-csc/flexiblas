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

/* Prototypes for external procedures generated from zlacon.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlacon_ (int *n, __GFORTRAN_DOUBLE_COMPLEX *v, __GFORTRAN_DOUBLE_COMPLEX *x, double *est, int *kase);

#ifdef __cplusplus
}
#endif
