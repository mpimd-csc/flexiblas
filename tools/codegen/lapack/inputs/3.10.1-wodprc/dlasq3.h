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

/* Prototypes for external procedures generated from dlasq3.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlasq3_ (int *i0, int *n0, double *z, int *pp, double *dmin, double *sigma, double *desig, double *qmax, int *nfail, int *iter, int *ndiv, int_least32_t *ieee, int *ttype, double *dmin1, double *dmin2, double *dn, double *dn1, double *dn2, double *g, double *tau);

#ifdef __cplusplus
}
#endif
