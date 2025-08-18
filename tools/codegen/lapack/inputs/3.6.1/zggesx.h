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

/* Prototypes for external procedures generated from zggesx.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zggesx_ (char *jobvsl, char *jobvsr, char *sort, int_least32_t *selctg, char *sense, int *n, __GFORTRAN_DOUBLE_COMPLEX *a, int *lda, __GFORTRAN_DOUBLE_COMPLEX *b, int *ldb, int *sdim, __GFORTRAN_DOUBLE_COMPLEX *alpha, __GFORTRAN_DOUBLE_COMPLEX *beta, __GFORTRAN_DOUBLE_COMPLEX *vsl, int *ldvsl, __GFORTRAN_DOUBLE_COMPLEX *vsr, int *ldvsr, double *rconde, double *rcondv, __GFORTRAN_DOUBLE_COMPLEX *work, int *lwork, double *rwork, int *iwork, int *liwork, int_least32_t *bwork, int *info, size_t jobvsl_len, size_t jobvsr_len, size_t sort_len, size_t sense_len);

#ifdef __cplusplus
}
#endif
