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

/* Prototypes for external procedures generated from zgtrfs.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zgtrfs_ (char *trans, int *n, int *nrhs, __GFORTRAN_DOUBLE_COMPLEX *dl, __GFORTRAN_DOUBLE_COMPLEX *d, __GFORTRAN_DOUBLE_COMPLEX *du, __GFORTRAN_DOUBLE_COMPLEX *dlf, __GFORTRAN_DOUBLE_COMPLEX *df, __GFORTRAN_DOUBLE_COMPLEX *duf, __GFORTRAN_DOUBLE_COMPLEX *du2, int *ipiv, __GFORTRAN_DOUBLE_COMPLEX *b, int *ldb, __GFORTRAN_DOUBLE_COMPLEX *x, int *ldx, double *ferr, double *berr, __GFORTRAN_DOUBLE_COMPLEX *work, double *rwork, int *info, size_t trans_len);

#ifdef __cplusplus
}
#endif
