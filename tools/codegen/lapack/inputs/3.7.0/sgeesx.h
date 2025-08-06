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

/* Prototypes for external procedures generated from sgeesx.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sgeesx_ (char *jobvs, char *sort, int_least32_t *select, char *sense, int *n, float *a, int *lda, int *sdim, float *wr, float *wi, float *vs, int *ldvs, float *rconde, float *rcondv, float *work, int *lwork, int *iwork, int *liwork, int_least32_t *bwork, int *info, size_t jobvs_len, size_t sort_len, size_t sense_len);

#ifdef __cplusplus
}
#endif
