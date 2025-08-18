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

/* Prototypes for external procedures generated from cggevx.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cggevx_ (char *balanc, char *jobvl, char *jobvr, char *sense, int *n, __GFORTRAN_FLOAT_COMPLEX *a, int *lda, __GFORTRAN_FLOAT_COMPLEX *b, int *ldb, __GFORTRAN_FLOAT_COMPLEX *alpha, __GFORTRAN_FLOAT_COMPLEX *beta, __GFORTRAN_FLOAT_COMPLEX *vl, int *ldvl, __GFORTRAN_FLOAT_COMPLEX *vr, int *ldvr, int *ilo, int *ihi, float *lscale, float *rscale, float *abnrm, float *bbnrm, float *rconde, float *rcondv, __GFORTRAN_FLOAT_COMPLEX *work, int *lwork, float *rwork, int *iwork, int_least32_t *bwork, int *info, size_t balanc_len, size_t jobvl_len, size_t jobvr_len, size_t sense_len);

#ifdef __cplusplus
}
#endif
