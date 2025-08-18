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

/* Prototypes for external procedures generated from cherfsx.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cherfsx_ (char *uplo, char *equed, int *n, int *nrhs, __GFORTRAN_FLOAT_COMPLEX *a, int *lda, __GFORTRAN_FLOAT_COMPLEX *af, int *ldaf, int *ipiv, float *s, __GFORTRAN_FLOAT_COMPLEX *b, int *ldb, __GFORTRAN_FLOAT_COMPLEX *x, int *ldx, float *rcond, float *berr, int *n_err_bnds, float *err_bnds_norm, float *err_bnds_comp, int *nparams, float *params, __GFORTRAN_FLOAT_COMPLEX *work, float *rwork, int *info, size_t uplo_len, size_t equed_len);

#ifdef __cplusplus
}
#endif
