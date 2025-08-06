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

/* Prototypes for external procedures generated from zlalsd.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlalsd_ (char *uplo, int *smlsiz, int *n, int *nrhs, double *d, double *e, __GFORTRAN_DOUBLE_COMPLEX *b, int *ldb, double *rcond, int *rank, __GFORTRAN_DOUBLE_COMPLEX *work, double *rwork, int *iwork, int *info, size_t uplo_len);

#ifdef __cplusplus
}
#endif
