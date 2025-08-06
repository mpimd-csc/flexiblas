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

/* Prototypes for external procedures generated from dlasd3.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlasd3_ (int *nl, int *nr, int *sqre, int *k, double *d, double *q, int *ldq, double *dsigma, double *u, int *ldu, double *u2, int *ldu2, double *vt, int *ldvt, double *vt2, int *ldvt2, int *idxc, int *ctot, double *z, int *info);

#ifdef __cplusplus
}
#endif
