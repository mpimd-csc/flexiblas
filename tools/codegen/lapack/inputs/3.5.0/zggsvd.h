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

/* Prototypes for external procedures generated from zggsvd.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zggsvd_ (char *jobu, char *jobv, char *jobq, int *m, int *n, int *p, int *k, int *l, __GFORTRAN_DOUBLE_COMPLEX *a, int *lda, __GFORTRAN_DOUBLE_COMPLEX *b, int *ldb, double *alpha, double *beta, __GFORTRAN_DOUBLE_COMPLEX *u, int *ldu, __GFORTRAN_DOUBLE_COMPLEX *v, int *ldv, __GFORTRAN_DOUBLE_COMPLEX *q, int *ldq, __GFORTRAN_DOUBLE_COMPLEX *work, double *rwork, int *iwork, int *info, size_t jobu_len, size_t jobv_len, size_t jobq_len);

#ifdef __cplusplus
}
#endif
