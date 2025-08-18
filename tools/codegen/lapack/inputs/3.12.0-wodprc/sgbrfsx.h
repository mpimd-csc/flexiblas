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

/* Prototypes for external procedures generated from sgbrfsx.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sgbrfsx_ (char *trans, char *equed, int *n, int *kl, int *ku, int *nrhs, float *ab, int *ldab, float *afb, int *ldafb, int *ipiv, float *r, float *c, float *b, int *ldb, float *x, int *ldx, float *rcond, float *berr, int *n_err_bnds, float *err_bnds_norm, float *err_bnds_comp, int *nparams, float *params, float *work, int *iwork, int *info, size_t trans_len, size_t equed_len);

#ifdef __cplusplus
}
#endif
