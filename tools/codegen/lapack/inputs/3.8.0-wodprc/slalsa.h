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

/* Prototypes for external procedures generated from slalsa.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slalsa_ (int *icompq, int *smlsiz, int *n, int *nrhs, float *b, int *ldb, float *bx, int *ldbx, float *u, int *ldu, float *vt, int *k, float *difl, float *difr, float *z, float *poles, int *givptr, int *givcol, int *ldgcol, int *perm, float *givnum, float *c, float *s, float *work, int *iwork, int *info);

#ifdef __cplusplus
}
#endif
