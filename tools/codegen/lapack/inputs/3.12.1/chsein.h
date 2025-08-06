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

/* Prototypes for external procedures generated from chsein.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void chsein_ (char *side, char *eigsrc, char *initv, int_least32_t *select, int *n, __GFORTRAN_FLOAT_COMPLEX *h, int *ldh, __GFORTRAN_FLOAT_COMPLEX *w, __GFORTRAN_FLOAT_COMPLEX *vl, int *ldvl, __GFORTRAN_FLOAT_COMPLEX *vr, int *ldvr, int *mm, int *m, __GFORTRAN_FLOAT_COMPLEX *work, float *rwork, int *ifaill, int *ifailr, int *info, size_t side_len, size_t eigsrc_len, size_t initv_len);

#ifdef __cplusplus
}
#endif
