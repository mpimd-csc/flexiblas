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

/* Prototypes for external procedures generated from dlaqr5.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlaqr5_ (int_least32_t *wantt, int_least32_t *wantz, int *kacc22, int *n, int *ktop, int *kbot, int *nshfts, double *sr, double *si, double *h, int *ldh, int *iloz, int *ihiz, double *z, int *ldz, double *v, int *ldv, double *u, int *ldu, int *nv, double *wv, int *ldwv, int *nh, double *wh, int *ldwh);

#ifdef __cplusplus
}
#endif
