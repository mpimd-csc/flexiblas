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

/* Prototypes for external procedures generated from sggevx.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sggevx_ (char *balanc, char *jobvl, char *jobvr, char *sense, int *n, float *a, int *lda, float *b, int *ldb, float *alphar, float *alphai, float *beta, float *vl, int *ldvl, float *vr, int *ldvr, int *ilo, int *ihi, float *lscale, float *rscale, float *abnrm, float *bbnrm, float *rconde, float *rcondv, float *work, int *lwork, int *iwork, int_least32_t *bwork, int *info, size_t balanc_len, size_t jobvl_len, size_t jobvr_len, size_t sense_len);

#ifdef __cplusplus
}
#endif
