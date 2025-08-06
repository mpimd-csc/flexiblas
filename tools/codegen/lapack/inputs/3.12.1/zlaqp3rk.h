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

/* Prototypes for external procedures generated from zlaqp3rk.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlaqp3rk_ (int *m, int *n, int *nrhs, int *ioffset, int *nb, double *abstol, double *reltol, int *kp1, double *maxc2nrm, __GFORTRAN_DOUBLE_COMPLEX *a, int *lda, int_least32_t *done, int *kb, double *maxc2nrmk, double *relmaxc2nrmk, int *jpiv, __GFORTRAN_DOUBLE_COMPLEX *tau, double *vn1, double *vn2, __GFORTRAN_DOUBLE_COMPLEX *auxv, __GFORTRAN_DOUBLE_COMPLEX *f, int *ldf, int *iwork, int *info);

#ifdef __cplusplus
}
#endif
