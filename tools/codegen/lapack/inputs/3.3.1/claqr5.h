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

/* Prototypes for external procedures generated from claqr5.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void claqr5_ (int_least32_t *wantt, int_least32_t *wantz, int *kacc22, int *n, int *ktop, int *kbot, int *nshfts, __GFORTRAN_FLOAT_COMPLEX *s, __GFORTRAN_FLOAT_COMPLEX *h, int *ldh, int *iloz, int *ihiz, __GFORTRAN_FLOAT_COMPLEX *z, int *ldz, __GFORTRAN_FLOAT_COMPLEX *v, int *ldv, __GFORTRAN_FLOAT_COMPLEX *u, int *ldu, int *nv, __GFORTRAN_FLOAT_COMPLEX *wv, int *ldwv, int *nh, __GFORTRAN_FLOAT_COMPLEX *wh, int *ldwh);

#ifdef __cplusplus
}
#endif
