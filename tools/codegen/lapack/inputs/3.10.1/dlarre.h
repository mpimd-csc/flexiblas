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

/* Prototypes for external procedures generated from dlarre.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlarre_ (char *range, int *n, double *vl, double *vu, int *il, int *iu, double *d, double *e, double *e2, double *rtol1, double *rtol2, double *spltol, int *nsplit, int *isplit, int *m, double *w, double *werr, double *wgap, int *iblock, int *indexw, double *gers, double *pivmin, double *work, int *iwork, int *info, size_t range_len);

#ifdef __cplusplus
}
#endif
