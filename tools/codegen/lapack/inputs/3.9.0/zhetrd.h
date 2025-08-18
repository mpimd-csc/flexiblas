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

/* Prototypes for external procedures generated from zhetrd.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zhetrd_ (char *uplo, int *n, __GFORTRAN_DOUBLE_COMPLEX *a, int *lda, double *d, double *e, __GFORTRAN_DOUBLE_COMPLEX *tau, __GFORTRAN_DOUBLE_COMPLEX *work, int *lwork, int *info, size_t uplo_len);

#ifdef __cplusplus
}
#endif
