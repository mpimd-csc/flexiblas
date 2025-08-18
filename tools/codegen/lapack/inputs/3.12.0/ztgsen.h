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

/* Prototypes for external procedures generated from ztgsen.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void ztgsen_ (int *ijob, int_least32_t *wantq, int_least32_t *wantz, int_least32_t *select, int *n, __GFORTRAN_DOUBLE_COMPLEX *a, int *lda, __GFORTRAN_DOUBLE_COMPLEX *b, int *ldb, __GFORTRAN_DOUBLE_COMPLEX *alpha, __GFORTRAN_DOUBLE_COMPLEX *beta, __GFORTRAN_DOUBLE_COMPLEX *q, int *ldq, __GFORTRAN_DOUBLE_COMPLEX *z, int *ldz, int *m, double *pl, double *pr, double *dif, __GFORTRAN_DOUBLE_COMPLEX *work, int *lwork, int *iwork, int *liwork, int *info);

#ifdef __cplusplus
}
#endif
