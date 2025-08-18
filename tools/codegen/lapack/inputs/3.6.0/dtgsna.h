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

/* Prototypes for external procedures generated from dtgsna.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dtgsna_ (char *job, char *howmny, int_least32_t *select, int *n, double *a, int *lda, double *b, int *ldb, double *vl, int *ldvl, double *vr, int *ldvr, double *s, double *dif, int *mm, int *m, double *work, int *lwork, int *iwork, int *info, size_t job_len, size_t howmny_len);

#ifdef __cplusplus
}
#endif
