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

/* Prototypes for external procedures generated from slaqr2.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slaqr2_ (int_least32_t *wantt, int_least32_t *wantz, int *n, int *ktop, int *kbot, int *nw, float *h, int *ldh, int *iloz, int *ihiz, float *z, int *ldz, int *ns, int *nd, float *sr, float *si, float *v, int *ldv, int *nh, float *t, int *ldt, int *nv, float *wv, int *ldwv, float *work, int *lwork);

#ifdef __cplusplus
}
#endif
