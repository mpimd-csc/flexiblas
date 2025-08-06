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

/* Prototypes for external procedures generated from dlarrd.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlarrd_ (char *range, char *order, int *n, double *vl, double *vu, int *il, int *iu, double *gers, double *reltol, double *d, double *e, double *e2, double *pivmin, int *nsplit, int *isplit, int *m, double *w, double *werr, double *wl, double *wu, int *iblock, int *indexw, double *work, int *iwork, int *info, size_t range_len, size_t order_len);

#ifdef __cplusplus
}
#endif
