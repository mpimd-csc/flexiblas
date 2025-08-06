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

/* Prototypes for external procedures generated from dgbrfsx.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dgbrfsx_ (char *trans, char *equed, int *n, int *kl, int *ku, int *nrhs, double *ab, int *ldab, double *afb, int *ldafb, int *ipiv, double *r, double *c, double *b, int *ldb, double *x, int *ldx, double *rcond, double *berr, int *n_err_bnds, double *err_bnds_norm, double *err_bnds_comp, int *nparams, double *params, double *work, int *iwork, int *info, size_t trans_len, size_t equed_len);

#ifdef __cplusplus
}
#endif
