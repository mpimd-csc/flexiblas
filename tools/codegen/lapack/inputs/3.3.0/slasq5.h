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

/* Prototypes for external procedures generated from slasq5.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slasq5_ (int *i0, int *n0, float *z, int *pp, float *tau, float *dmin, float *dmin1, float *dmin2, float *dn, float *dnm1, float *dnm2, int_least32_t *ieee);

#ifdef __cplusplus
}
#endif
