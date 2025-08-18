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

/* Prototypes for external procedures generated from dlaqz0.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlaqz0_ (const char *wants, const char *wantq, const char *wantz, const int *n, const int *ilo, const int *ihi, double *a, const int *lda, double *b, const int *ldb, double *alphar, double *alphai, double *beta, double *q, const int *ldq, double *z, const int *ldz, double *work, const int *lwork, const int *rec, int *info, size_t wants_len, size_t wantq_len, size_t wantz_len);

#ifdef __cplusplus
}
#endif
