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

/* Prototypes for external procedures generated from sposvx.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sposvx_ (char *fact, char *uplo, int *n, int *nrhs, float *a, int *lda, float *af, int *ldaf, char *equed, float *s, float *b, int *ldb, float *x, int *ldx, float *rcond, float *ferr, float *berr, float *work, int *iwork, int *info, size_t fact_len, size_t uplo_len, size_t equed_len);

#ifdef __cplusplus
}
#endif
