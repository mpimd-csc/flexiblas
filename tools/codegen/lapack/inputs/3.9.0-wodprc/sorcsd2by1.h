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

/* Prototypes for external procedures generated from sorcsd2by1.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sorcsd2by1_ (char *jobu1, char *jobu2, char *jobv1t, int *m, int *p, int *q, float *x11, int *ldx11, float *x21, int *ldx21, float *theta, float *u1, int *ldu1, float *u2, int *ldu2, float *v1t, int *ldv1t, float *work, int *lwork, int *iwork, int *info, size_t jobu1_len, size_t jobu2_len, size_t jobv1t_len);

#ifdef __cplusplus
}
#endif
