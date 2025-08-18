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

/* Prototypes for external procedures generated from dlaebz.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlaebz_ (int *ijob, int *nitmax, int *n, int *mmax, int *minp, int *nbmin, double *abstol, double *reltol, double *pivmin, double *d, double *e, double *e2, int *nval, double *ab, double *c, int *mout, int *nab, double *work, int *iwork, int *info);

#ifdef __cplusplus
}
#endif
