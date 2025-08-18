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

/* Prototypes for external procedures generated from dggsvp.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dggsvp_ (char *jobu, char *jobv, char *jobq, int *m, int *p, int *n, double *a, int *lda, double *b, int *ldb, double *tola, double *tolb, int *k, int *l, double *u, int *ldu, double *v, int *ldv, double *q, int *ldq, int *iwork, double *tau, double *work, int *info, size_t jobu_len, size_t jobv_len, size_t jobq_len);

#ifdef __cplusplus
}
#endif
