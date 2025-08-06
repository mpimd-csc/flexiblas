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

/* Prototypes for external procedures generated from zla_gbrfsx_extended.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zla_gbrfsx_extended_ (int *prec_type, int *trans_type, int *n, int *kl, int *ku, int *nrhs, __GFORTRAN_DOUBLE_COMPLEX *ab, int *ldab, __GFORTRAN_DOUBLE_COMPLEX *afb, int *ldafb, int *ipiv, int_least32_t *colequ, double *c, __GFORTRAN_DOUBLE_COMPLEX *b, int *ldb, __GFORTRAN_DOUBLE_COMPLEX *y, int *ldy, double *berr_out, int *n_norms, double *err_bnds_norm, double *err_bnds_comp, __GFORTRAN_DOUBLE_COMPLEX *res, double *ayb, __GFORTRAN_DOUBLE_COMPLEX *dy, __GFORTRAN_DOUBLE_COMPLEX *y_tail, double *rcond, int *ithresh, double *rthresh, double *dz_ub, int_least32_t *ignore_cwise, int *info);

#ifdef __cplusplus
}
#endif
