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

/* Prototypes for external procedures generated from dlar1v.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlar1v_ (int *n, int *b1, int *bn, double *lambda, double *d, double *l, double *ld, double *lld, double *pivmin, double *gaptol, double *z, int_least32_t *wantnc, int *negcnt, double *ztz, double *mingma, int *r, int *isuppz, double *nrminv, double *resid, double *rqcorr, double *work);

#ifdef __cplusplus
}
#endif
