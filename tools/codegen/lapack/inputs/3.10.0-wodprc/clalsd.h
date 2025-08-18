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

/* Prototypes for external procedures generated from clalsd.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void clalsd_ (char *uplo, int *smlsiz, int *n, int *nrhs, float *d, float *e, __GFORTRAN_FLOAT_COMPLEX *b, int *ldb, float *rcond, int *rank, __GFORTRAN_FLOAT_COMPLEX *work, float *rwork, int *iwork, int *info, size_t uplo_len);

#ifdef __cplusplus
}
#endif
