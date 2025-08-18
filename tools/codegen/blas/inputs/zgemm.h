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

/* Prototypes for external procedures generated from zgemm.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zgemm_ (char *transa, char *transb, int *m, int *n, int *k, __GFORTRAN_DOUBLE_COMPLEX *alpha, __GFORTRAN_DOUBLE_COMPLEX *a, int *lda, __GFORTRAN_DOUBLE_COMPLEX *b, int *ldb, __GFORTRAN_DOUBLE_COMPLEX *beta, __GFORTRAN_DOUBLE_COMPLEX *c, int *ldc, size_t transa_len, size_t transb_len);

#ifdef __cplusplus
}
#endif
