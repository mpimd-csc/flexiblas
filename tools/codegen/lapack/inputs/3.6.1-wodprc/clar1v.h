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

/* Prototypes for external procedures generated from clar1v.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void clar1v_ (int *n, int *b1, int *bn, float *lambda, float *d, float *l, float *ld, float *lld, float *pivmin, float *gaptol, __GFORTRAN_FLOAT_COMPLEX *z, int_least32_t *wantnc, int *negcnt, float *ztz, float *mingma, int *r, int *isuppz, float *nrminv, float *resid, float *rqcorr, float *work);

#ifdef __cplusplus
}
#endif
