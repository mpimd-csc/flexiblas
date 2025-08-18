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

/* Prototypes for external procedures generated from slaln2.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slaln2_ (int_least32_t *ltrans, int *na, int *nw, float *smin, float *ca, float *a, int *lda, float *d1, float *d2, float *b, int *ldb, float *wr, float *wi, float *x, int *ldx, float *scale, float *xnorm, int *info);

#ifdef __cplusplus
}
#endif
