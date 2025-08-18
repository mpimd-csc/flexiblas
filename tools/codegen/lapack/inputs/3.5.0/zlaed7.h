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

/* Prototypes for external procedures generated from zlaed7.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlaed7_ (int *n, int *cutpnt, int *qsiz, int *tlvls, int *curlvl, int *curpbm, double *d, __GFORTRAN_DOUBLE_COMPLEX *q, int *ldq, double *rho, int *indxq, double *qstore, int *qptr, int *prmptr, int *perm, int *givptr, int *givcol, double *givnum, __GFORTRAN_DOUBLE_COMPLEX *work, double *rwork, int *iwork, int *info);

#ifdef __cplusplus
}
#endif
