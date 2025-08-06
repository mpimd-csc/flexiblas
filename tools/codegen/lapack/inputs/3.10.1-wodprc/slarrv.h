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

/* Prototypes for external procedures generated from slarrv.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slarrv_ (int *n, float *vl, float *vu, float *d, float *l, float *pivmin, int *isplit, int *m, int *dol, int *dou, float *minrgp, float *rtol1, float *rtol2, float *w, float *werr, float *wgap, int *iblock, int *indexw, float *gers, float *z, int *ldz, int *isuppz, float *work, int *iwork, int *info);

#ifdef __cplusplus
}
#endif
