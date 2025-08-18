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

/* Prototypes for external procedures generated from cla_gerfsx_extended.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cla_gerfsx_extended_ (int *prec_type, int *trans_type, int *n, int *nrhs, __GFORTRAN_FLOAT_COMPLEX *a, int *lda, __GFORTRAN_FLOAT_COMPLEX *af, int *ldaf, int *ipiv, int_least32_t *colequ, float *c, __GFORTRAN_FLOAT_COMPLEX *b, int *ldb, __GFORTRAN_FLOAT_COMPLEX *y, int *ldy, float *berr_out, int *n_norms, float *errs_n, float *errs_c, __GFORTRAN_FLOAT_COMPLEX *res, float *ayb, __GFORTRAN_FLOAT_COMPLEX *dy, __GFORTRAN_FLOAT_COMPLEX *y_tail, float *rcond, int *ithresh, float *rthresh, float *dz_ub, int_least32_t *ignore_cwise, int *info);

#ifdef __cplusplus
}
#endif
