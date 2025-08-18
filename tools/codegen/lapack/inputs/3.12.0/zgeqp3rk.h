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

/* Prototypes for external procedures generated from zgeqp3rk.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zgeqp3rk_ (int *m, int *n, int *nrhs, int *kmax, double *abstol, double *reltol, __GFORTRAN_DOUBLE_COMPLEX *a, int *lda, int *k, double *maxc2nrmk, double *relmaxc2nrmk, int *jpiv, __GFORTRAN_DOUBLE_COMPLEX *tau, __GFORTRAN_DOUBLE_COMPLEX *work, int *lwork, double *rwork, int *iwork, int *info);

#ifdef __cplusplus
}
#endif
