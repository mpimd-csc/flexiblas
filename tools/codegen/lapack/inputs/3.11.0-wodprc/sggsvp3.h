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

/* Prototypes for external procedures generated from sggsvp3.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sggsvp3_ (char *jobu, char *jobv, char *jobq, int *m, int *p, int *n, float *a, int *lda, float *b, int *ldb, float *tola, float *tolb, int *k, int *l, float *u, int *ldu, float *v, int *ldv, float *q, int *ldq, int *iwork, float *tau, float *work, int *lwork, int *info, size_t jobu_len, size_t jobv_len, size_t jobq_len);

#ifdef __cplusplus
}
#endif
