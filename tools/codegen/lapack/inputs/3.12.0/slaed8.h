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

/* Prototypes for external procedures generated from slaed8.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slaed8_ (int *icompq, int *k, int *n, int *qsiz, float *d, float *q, int *ldq, int *indxq, float *rho, int *cutpnt, float *z, float *dlambda, float *q2, int *ldq2, float *w, int *perm, int *givptr, int *givcol, float *givnum, int *indxp, int *indx, int *info);

#ifdef __cplusplus
}
#endif
