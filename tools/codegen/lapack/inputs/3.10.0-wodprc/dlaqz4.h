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

/* Prototypes for external procedures generated from dlaqz4.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlaqz4_ (const int_least32_t *ilschur, const int_least32_t *ilq, const int_least32_t *ilz, const int *n, const int *ilo, const int *ihi, const int *nshifts, const int *nblock_desired, double *sr, double *si, double *ss, double *a, const int *lda, double *b, const int *ldb, double *q, const int *ldq, double *z, const int *ldz, double *qc, const int *ldqc, double *zc, const int *ldzc, double *work, const int *lwork, int *info);

#ifdef __cplusplus
}
#endif
