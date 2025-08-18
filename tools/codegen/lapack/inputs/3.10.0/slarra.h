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

/* Prototypes for external procedures generated from slarra.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slarra_ (int *n, float *d, float *e, float *e2, float *spltol, float *tnrm, int *nsplit, int *isplit, int *info);

#ifdef __cplusplus
}
#endif
