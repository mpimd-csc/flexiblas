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

/* Prototypes for external procedures generated from dlaed8.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlaed8_ (int *icompq, int *k, int *n, int *qsiz, double *d, double *q, int *ldq, int *indxq, double *rho, int *cutpnt, double *z, double *dlambda, double *q2, int *ldq2, double *w, int *perm, int *givptr, int *givcol, double *givnum, int *indxp, int *indx, int *info);

#ifdef __cplusplus
}
#endif
