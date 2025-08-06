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

/* Prototypes for external procedures generated from cuncsd.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cuncsd_ (char *jobu1, char *jobu2, char *jobv1t, char *jobv2t, char *trans, char *signs, int *m, int *p, int *q, __GFORTRAN_FLOAT_COMPLEX *x11, int *ldx11, __GFORTRAN_FLOAT_COMPLEX *x12, int *ldx12, __GFORTRAN_FLOAT_COMPLEX *x21, int *ldx21, __GFORTRAN_FLOAT_COMPLEX *x22, int *ldx22, float *theta, __GFORTRAN_FLOAT_COMPLEX *u1, int *ldu1, __GFORTRAN_FLOAT_COMPLEX *u2, int *ldu2, __GFORTRAN_FLOAT_COMPLEX *v1t, int *ldv1t, __GFORTRAN_FLOAT_COMPLEX *v2t, int *ldv2t, __GFORTRAN_FLOAT_COMPLEX *work, int *lwork, float *rwork, int *lrwork, int *iwork, int *info, size_t jobu1_len, size_t jobu2_len, size_t jobv1t_len, size_t jobv2t_len, size_t trans_len, size_t signs_len);

#ifdef __cplusplus
}
#endif
