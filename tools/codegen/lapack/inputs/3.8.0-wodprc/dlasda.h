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

/* Prototypes for external procedures generated from dlasda.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlasda_ (int *icompq, int *smlsiz, int *n, int *sqre, double *d, double *e, double *u, int *ldu, double *vt, int *k, double *difl, double *difr, double *z, double *poles, int *givptr, int *givcol, int *ldgcol, int *perm, double *givnum, double *c, double *s, double *work, int *iwork, int *info);

#ifdef __cplusplus
}
#endif
