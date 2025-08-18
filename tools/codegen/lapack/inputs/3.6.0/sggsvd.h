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

/* Prototypes for external procedures generated from sggsvd.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sggsvd_ (char *jobu, char *jobv, char *jobq, int *m, int *n, int *p, int *k, int *l, float *a, int *lda, float *b, int *ldb, float *alpha, float *beta, float *u, int *ldu, float *v, int *ldv, float *q, int *ldq, float *work, int *iwork, int *info, size_t jobu_len, size_t jobv_len, size_t jobq_len);

#ifdef __cplusplus
}
#endif
