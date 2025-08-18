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

/* Prototypes for external procedures generated from dgedmdq.f90

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dgedmdq_ (const char *jobs, const char *jobz, const char *jobr, const char *jobq, const char *jobt, const char *jobf, const int *whtsvd, const int *m, const int *n, double *f, const int *ldf, double *x, const int *ldx, double *y, const int *ldy, const int *nrnk, const double *tol, int *k, double *reig, double *imeig, double *z, const int *ldz, double *res, double *b, const int *ldb, double *v, const int *ldv, double *s, const int *lds, double *work, const int *lwork, int *iwork, const int *liwork, int *info, size_t jobs_len, size_t jobz_len, size_t jobr_len, size_t jobq_len, size_t jobt_len, size_t jobf_len);

#ifdef __cplusplus
}
#endif
