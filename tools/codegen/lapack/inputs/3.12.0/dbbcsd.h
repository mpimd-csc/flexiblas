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

/* Prototypes for external procedures generated from dbbcsd.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dbbcsd_ (char *jobu1, char *jobu2, char *jobv1t, char *jobv2t, char *trans, int *m, int *p, int *q, double *theta, double *phi, double *u1, int *ldu1, double *u2, int *ldu2, double *v1t, int *ldv1t, double *v2t, int *ldv2t, double *b11d, double *b11e, double *b12d, double *b12e, double *b21d, double *b21e, double *b22d, double *b22e, double *work, int *lwork, int *info, size_t jobu1_len, size_t jobu2_len, size_t jobv1t_len, size_t jobv2t_len, size_t trans_len);

#ifdef __cplusplus
}
#endif
