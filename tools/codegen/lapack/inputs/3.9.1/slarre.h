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

/* Prototypes for external procedures generated from slarre.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slarre_ (char *range, int *n, float *vl, float *vu, int *il, int *iu, float *d, float *e, float *e2, float *rtol1, float *rtol2, float *spltol, int *nsplit, int *isplit, int *m, float *w, float *werr, float *wgap, int *iblock, int *indexw, float *gers, float *pivmin, float *work, int *iwork, int *info, size_t range_len);

#ifdef __cplusplus
}
#endif
