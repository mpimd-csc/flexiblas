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

/* Prototypes for external procedures generated from zlahqr.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlahqr_ (int_least32_t *wantt, int_least32_t *wantz, int *n, int *ilo, int *ihi, __GFORTRAN_DOUBLE_COMPLEX *h, int *ldh, __GFORTRAN_DOUBLE_COMPLEX *w, int *iloz, int *ihiz, __GFORTRAN_DOUBLE_COMPLEX *z, int *ldz, int *info);

#ifdef __cplusplus
}
#endif
