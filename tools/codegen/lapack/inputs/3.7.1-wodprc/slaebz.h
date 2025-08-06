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

/* Prototypes for external procedures generated from slaebz.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slaebz_ (int *ijob, int *nitmax, int *n, int *mmax, int *minp, int *nbmin, float *abstol, float *reltol, float *pivmin, float *d, float *e, float *e2, int *nval, float *ab, float *c, int *mout, int *nab, float *work, int *iwork, int *info);

#ifdef __cplusplus
}
#endif
