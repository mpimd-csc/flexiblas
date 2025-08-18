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

/* Prototypes for external procedures generated from stgsyl.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void stgsyl_ (char *trans, int *ijob, int *m, int *n, float *a, int *lda, float *b, int *ldb, float *c, int *ldc, float *d, int *ldd, float *e, int *lde, float *f, int *ldf, float *scale, float *dif, float *work, int *lwork, int *iwork, int *info, size_t trans_len);

#ifdef __cplusplus
}
#endif
