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

/* Prototypes for external procedures generated from slals0.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slals0_ (int *icompq, int *nl, int *nr, int *sqre, int *nrhs, float *b, int *ldb, float *bx, int *ldbx, int *perm, int *givptr, int *givcol, int *ldgcol, float *givnum, int *ldgnum, float *poles, float *difl, float *difr, float *z, int *k, float *c, float *s, float *work, int *info);

#ifdef __cplusplus
}
#endif
