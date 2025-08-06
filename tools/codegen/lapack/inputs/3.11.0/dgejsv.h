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

/* Prototypes for external procedures generated from dgejsv.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dgejsv_ (char *joba, char *jobu, char *jobv, char *jobr, char *jobt, char *jobp, int *m, int *n, double *a, int *lda, double *sva, double *u, int *ldu, double *v, int *ldv, double *work, int *lwork, int *iwork, int *info, size_t joba_len, size_t jobu_len, size_t jobv_len, size_t jobr_len, size_t jobt_len, size_t jobp_len);

#ifdef __cplusplus
}
#endif
