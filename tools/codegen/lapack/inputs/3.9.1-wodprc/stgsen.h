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

/* Prototypes for external procedures generated from stgsen.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void stgsen_ (int *ijob, int_least32_t *wantq, int_least32_t *wantz, int_least32_t *select, int *n, float *a, int *lda, float *b, int *ldb, float *alphar, float *alphai, float *beta, float *q, int *ldq, float *z, int *ldz, int *m, float *pl, float *pr, float *dif, float *work, int *lwork, int *iwork, int *liwork, int *info);

#ifdef __cplusplus
}
#endif
