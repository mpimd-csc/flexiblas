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

/* Prototypes for external procedures generated from zlaqr3.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlaqr3_ (int_least32_t *wantt, int_least32_t *wantz, int *n, int *ktop, int *kbot, int *nw, __GFORTRAN_DOUBLE_COMPLEX *h, int *ldh, int *iloz, int *ihiz, __GFORTRAN_DOUBLE_COMPLEX *z, int *ldz, int *ns, int *nd, __GFORTRAN_DOUBLE_COMPLEX *sh, __GFORTRAN_DOUBLE_COMPLEX *v, int *ldv, int *nh, __GFORTRAN_DOUBLE_COMPLEX *t, int *ldt, int *nv, __GFORTRAN_DOUBLE_COMPLEX *wv, int *ldwv, __GFORTRAN_DOUBLE_COMPLEX *work, int *lwork);

#ifdef __cplusplus
}
#endif
