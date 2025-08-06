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

/* Prototypes for external procedures generated from slaqz3.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slaqz3_ (const int_least32_t *ilschur, const int_least32_t *ilq, const int_least32_t *ilz, const int *n, const int *ilo, const int *ihi, const int *nw, float *a, const int *lda, float *b, const int *ldb, float *q, const int *ldq, float *z, const int *ldz, int *ns, int *nd, float *alphar, float *alphai, float *beta, float *qc, const int *ldqc, float *zc, const int *ldzc, float *work, const int *lwork, const int *rec, int *info);

#ifdef __cplusplus
}
#endif
