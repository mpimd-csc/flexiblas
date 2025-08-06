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

/* Prototypes for external procedures generated from zgedmd.f90

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zgedmd_ (const char *jobs, const char *jobz, const char *jobr, const char *jobf, const int *whtsvd, const int *m, const int *n, __GFORTRAN_DOUBLE_COMPLEX *x, const int *ldx, __GFORTRAN_DOUBLE_COMPLEX *y, const int *ldy, const int *nrnk, const double *tol, int *k, __GFORTRAN_DOUBLE_COMPLEX *eigs, __GFORTRAN_DOUBLE_COMPLEX *z, const int *ldz, double *res, __GFORTRAN_DOUBLE_COMPLEX *b, const int *ldb, __GFORTRAN_DOUBLE_COMPLEX *w, const int *ldw, __GFORTRAN_DOUBLE_COMPLEX *s, const int *lds, __GFORTRAN_DOUBLE_COMPLEX *zwork, const int *lzwork, double *rwork, const int *lrwork, int *iwork, const int *liwork, int *info, size_t jobs_len, size_t jobz_len, size_t jobr_len, size_t jobf_len);

#ifdef __cplusplus
}
#endif
