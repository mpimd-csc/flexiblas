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

/* Prototypes for external procedures generated from slasd2.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slasd2_ (int *nl, int *nr, int *sqre, int *k, float *d, float *z, float *alpha, float *beta, float *u, int *ldu, float *vt, int *ldvt, float *dsigma, float *u2, int *ldu2, float *vt2, int *ldvt2, int *idxp, int *idx, int *idxc, int *idxq, int *coltyp, int *info);

#ifdef __cplusplus
}
#endif
