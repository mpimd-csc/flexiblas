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

/* Prototypes for external procedures generated from sgedmdq.f90

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sgedmdq_ (const char *jobs, const char *jobz, const char *jobr, const char *jobq, const char *jobt, const char *jobf, const int *whtsvd, const int *m, const int *n, float *f, const int *ldf, float *x, const int *ldx, float *y, const int *ldy, const int *nrnk, const float *tol, int *k, float *reig, float *imeig, float *z, const int *ldz, float *res, float *b, const int *ldb, float *v, const int *ldv, float *s, const int *lds, float *work, const int *lwork, int *iwork, const int *liwork, int *info, size_t jobs_len, size_t jobz_len, size_t jobr_len, size_t jobq_len, size_t jobt_len, size_t jobf_len);

#ifdef __cplusplus
}
#endif
