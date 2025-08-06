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

/* Prototypes for external procedures generated from slaqp2rk.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slaqp2rk_ (int *m, int *n, int *nrhs, int *ioffset, int *kmax, float *abstol, float *reltol, int *kp1, float *maxc2nrm, float *a, int *lda, int *k, float *maxc2nrmk, float *relmaxc2nrmk, int *jpiv, float *tau, float *vn1, float *vn2, float *work, int *info);

#ifdef __cplusplus
}
#endif
