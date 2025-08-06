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

/* Prototypes for external procedures generated from dtgsy2.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dtgsy2_ (char *trans, int *ijob, int *m, int *n, double *a, int *lda, double *b, int *ldb, double *c, int *ldc, double *d, int *ldd, double *e, int *lde, double *f, int *ldf, double *scale, double *rdsum, double *rdscal, int *iwork, int *pq, int *info, size_t trans_len);

#ifdef __cplusplus
}
#endif
