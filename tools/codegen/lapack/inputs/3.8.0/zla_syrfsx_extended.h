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

/* Prototypes for external procedures generated from zla_syrfsx_extended.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zla_syrfsx_extended_ (int *prec_type, char *uplo, int *n, int *nrhs, __GFORTRAN_DOUBLE_COMPLEX *a, int *lda, __GFORTRAN_DOUBLE_COMPLEX *af, int *ldaf, int *ipiv, int_least32_t *colequ, double *c, __GFORTRAN_DOUBLE_COMPLEX *b, int *ldb, __GFORTRAN_DOUBLE_COMPLEX *y, int *ldy, double *berr_out, int *n_norms, double *err_bnds_norm, double *err_bnds_comp, __GFORTRAN_DOUBLE_COMPLEX *res, double *ayb, __GFORTRAN_DOUBLE_COMPLEX *dy, __GFORTRAN_DOUBLE_COMPLEX *y_tail, double *rcond, int *ithresh, double *rthresh, double *dz_ub, int_least32_t *ignore_cwise, int *info, size_t uplo_len);

#ifdef __cplusplus
}
#endif
