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

/* Prototypes for external procedures generated from zgesvxx.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zgesvxx_ (char *fact, char *trans, int *n, int *nrhs, __GFORTRAN_DOUBLE_COMPLEX *a, int *lda, __GFORTRAN_DOUBLE_COMPLEX *af, int *ldaf, int *ipiv, char *equed, double *r, double *c, __GFORTRAN_DOUBLE_COMPLEX *b, int *ldb, __GFORTRAN_DOUBLE_COMPLEX *x, int *ldx, double *rcond, double *rpvgrw, double *berr, int *n_err_bnds, double *err_bnds_norm, double *err_bnds_comp, int *nparams, double *params, __GFORTRAN_DOUBLE_COMPLEX *work, double *rwork, int *info, size_t fact_len, size_t trans_len, size_t equed_len);

#ifdef __cplusplus
}
#endif
