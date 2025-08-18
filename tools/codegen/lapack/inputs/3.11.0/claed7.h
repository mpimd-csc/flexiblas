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

/* Prototypes for external procedures generated from claed7.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void claed7_ (int *n, int *cutpnt, int *qsiz, int *tlvls, int *curlvl, int *curpbm, float *d, __GFORTRAN_FLOAT_COMPLEX *q, int *ldq, float *rho, int *indxq, float *qstore, int *qptr, int *prmptr, int *perm, int *givptr, int *givcol, float *givnum, __GFORTRAN_FLOAT_COMPLEX *work, float *rwork, int *iwork, int *info);

#ifdef __cplusplus
}
#endif
