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

/* Prototypes for external procedures generated from claed8.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void claed8_ (int *k, int *n, int *qsiz, __GFORTRAN_FLOAT_COMPLEX *q, int *ldq, float *d, float *rho, int *cutpnt, float *z, float *dlamda, __GFORTRAN_FLOAT_COMPLEX *q2, int *ldq2, float *w, int *indxp, int *indx, int *indxq, int *perm, int *givptr, int *givcol, float *givnum, int *info);

#ifdef __cplusplus
}
#endif
