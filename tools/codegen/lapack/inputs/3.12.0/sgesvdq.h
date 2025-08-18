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

/* Prototypes for external procedures generated from sgesvdq.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sgesvdq_ (char *joba, char *jobp, char *jobr, char *jobu, char *jobv, int *m, int *n, float *a, int *lda, float *s, float *u, int *ldu, float *v, int *ldv, int *numrank, int *iwork, int *liwork, float *work, int *lwork, float *rwork, int *lrwork, int *info, size_t joba_len, size_t jobp_len, size_t jobr_len, size_t jobu_len, size_t jobv_len);

#ifdef __cplusplus
}
#endif
