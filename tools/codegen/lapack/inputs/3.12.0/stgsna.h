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

/* Prototypes for external procedures generated from stgsna.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void stgsna_ (char *job, char *howmny, int_least32_t *select, int *n, float *a, int *lda, float *b, int *ldb, float *vl, int *ldvl, float *vr, int *ldvr, float *s, float *dif, int *mm, int *m, float *work, int *lwork, int *iwork, int *info, size_t job_len, size_t howmny_len);

#ifdef __cplusplus
}
#endif
