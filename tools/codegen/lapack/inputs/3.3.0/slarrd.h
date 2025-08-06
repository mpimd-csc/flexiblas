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

/* Prototypes for external procedures generated from slarrd.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slarrd_ (char *range, char *order, int *n, float *vl, float *vu, int *il, int *iu, float *gers, float *reltol, float *d, float *e, float *e2, float *pivmin, int *nsplit, int *isplit, int *m, float *w, float *werr, float *wl, float *wu, int *iblock, int *indexw, float *work, int *iwork, int *info, size_t range_len, size_t order_len);

#ifdef __cplusplus
}
#endif
