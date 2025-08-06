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

/* Prototypes for external procedures generated from claqz1.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void claqz1_ (const int_least32_t *ilq, const int_least32_t *ilz, const int *k, const int *istartm, const int *istopm, const int *ihi, __GFORTRAN_FLOAT_COMPLEX *a, const int *lda, __GFORTRAN_FLOAT_COMPLEX *b, const int *ldb, const int *nq, const int *qstart, __GFORTRAN_FLOAT_COMPLEX *q, const int *ldq, const int *nz, const int *zstart, __GFORTRAN_FLOAT_COMPLEX *z, const int *ldz);

#ifdef __cplusplus
}
#endif
