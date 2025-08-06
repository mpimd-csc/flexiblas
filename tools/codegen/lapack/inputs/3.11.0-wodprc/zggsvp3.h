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

/* Prototypes for external procedures generated from zggsvp3.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zggsvp3_ (char *jobu, char *jobv, char *jobq, int *m, int *p, int *n, __GFORTRAN_DOUBLE_COMPLEX *a, int *lda, __GFORTRAN_DOUBLE_COMPLEX *b, int *ldb, double *tola, double *tolb, int *k, int *l, __GFORTRAN_DOUBLE_COMPLEX *u, int *ldu, __GFORTRAN_DOUBLE_COMPLEX *v, int *ldv, __GFORTRAN_DOUBLE_COMPLEX *q, int *ldq, int *iwork, double *rwork, __GFORTRAN_DOUBLE_COMPLEX *tau, __GFORTRAN_DOUBLE_COMPLEX *work, int *lwork, int *info, size_t jobu_len, size_t jobv_len, size_t jobq_len);

#ifdef __cplusplus
}
#endif
