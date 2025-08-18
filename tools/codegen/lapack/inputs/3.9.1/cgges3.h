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

/* Prototypes for external procedures generated from cgges3.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cgges3_ (char *jobvsl, char *jobvsr, char *sort, int_least32_t *selctg, int *n, __GFORTRAN_FLOAT_COMPLEX *a, int *lda, __GFORTRAN_FLOAT_COMPLEX *b, int *ldb, int *sdim, __GFORTRAN_FLOAT_COMPLEX *alpha, __GFORTRAN_FLOAT_COMPLEX *beta, __GFORTRAN_FLOAT_COMPLEX *vsl, int *ldvsl, __GFORTRAN_FLOAT_COMPLEX *vsr, int *ldvsr, __GFORTRAN_FLOAT_COMPLEX *work, int *lwork, float *rwork, int_least32_t *bwork, int *info, size_t jobvsl_len, size_t jobvsr_len, size_t sort_len);

#ifdef __cplusplus
}
#endif
