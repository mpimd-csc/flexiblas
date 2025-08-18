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

/* Prototypes for external procedures generated from sgges.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sgges_ (char *jobvsl, char *jobvsr, char *sort, int_least32_t *selctg, int *n, float *a, int *lda, float *b, int *ldb, int *sdim, float *alphar, float *alphai, float *beta, float *vsl, int *ldvsl, float *vsr, int *ldvsr, float *work, int *lwork, int_least32_t *bwork, int *info, size_t jobvsl_len, size_t jobvsr_len, size_t sort_len);

#ifdef __cplusplus
}
#endif
