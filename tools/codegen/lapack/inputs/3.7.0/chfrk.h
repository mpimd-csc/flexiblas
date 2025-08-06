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

/* Prototypes for external procedures generated from chfrk.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void chfrk_ (char *transr, char *uplo, char *trans, int *n, int *k, float *alpha, __GFORTRAN_FLOAT_COMPLEX *a, int *lda, float *beta, __GFORTRAN_FLOAT_COMPLEX *c, size_t transr_len, size_t uplo_len, size_t trans_len);

#ifdef __cplusplus
}
#endif
