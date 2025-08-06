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

/* Prototypes for external procedures generated from dlasd6.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlasd6_ (int *icompq, int *nl, int *nr, int *sqre, double *d, double *vf, double *vl, double *alpha, double *beta, int *idxq, int *perm, int *givptr, int *givcol, int *ldgcol, double *givnum, int *ldgnum, double *poles, double *difl, double *difr, double *z, int *k, double *c, double *s, double *work, int *iwork, int *info);

#ifdef __cplusplus
}
#endif
