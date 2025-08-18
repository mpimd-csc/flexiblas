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

/* Prototypes for external procedures generated from stptrs.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void stptrs_ (char *uplo, char *trans, char *diag, int *n, int *nrhs, float *ap, float *b, int *ldb, int *info, size_t uplo_len, size_t trans_len, size_t diag_len);

#ifdef __cplusplus
}
#endif
