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

/* Prototypes for external procedures generated from dlasd4.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlasd4_ (int *n, int *i, double *d, double *z, double *delta, double *rho, double *sigma, double *work, int *info);

#ifdef __cplusplus
}
#endif
