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

/* Prototypes for external procedures generated from claein.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void claein_ (int_least32_t *rightv, int_least32_t *noinit, int *n, __GFORTRAN_FLOAT_COMPLEX *h, int *ldh, __GFORTRAN_FLOAT_COMPLEX *w, __GFORTRAN_FLOAT_COMPLEX *v, __GFORTRAN_FLOAT_COMPLEX *b, int *ldb, float *rwork, float *eps3, float *smlnum, int *info);

#ifdef __cplusplus
}
#endif
