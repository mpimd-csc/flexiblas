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

/* Prototypes for external procedures generated from sla_syrfsx_extended.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sla_syrfsx_extended_ (int *prec_type, char *uplo, int *n, int *nrhs, float *a, int *lda, float *af, int *ldaf, int *ipiv, int_least32_t *colequ, float *c, float *b, int *ldb, float *y, int *ldy, float *berr_out, int *n_norms, float *err_bnds_norm, float *err_bnds_comp, float *res, float *ayb, float *dy, float *y_tail, float *rcond, int *ithresh, float *rthresh, float *dz_ub, int_least32_t *ignore_cwise, int *info, size_t uplo_len);

#ifdef __cplusplus
}
#endif
