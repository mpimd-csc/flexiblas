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

/* Prototypes for external procedures generated from zlarrv.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlarrv_ (int *n, double *vl, double *vu, double *d, double *l, double *pivmin, int *isplit, int *m, int *dol, int *dou, double *minrgp, double *rtol1, double *rtol2, double *w, double *werr, double *wgap, int *iblock, int *indexw, double *gers, __GFORTRAN_DOUBLE_COMPLEX *z, int *ldz, int *isuppz, double *work, int *iwork, int *info);

#ifdef __cplusplus
}
#endif
