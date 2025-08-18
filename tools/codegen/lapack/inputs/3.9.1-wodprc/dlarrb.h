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

/* Prototypes for external procedures generated from dlarrb.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlarrb_ (int *n, double *d, double *lld, int *ifirst, int *ilast, double *rtol1, double *rtol2, int *offset, double *w, double *wgap, double *werr, double *work, int *iwork, double *pivmin, double *spdiam, int *twist, int *info);

#ifdef __cplusplus
}
#endif
