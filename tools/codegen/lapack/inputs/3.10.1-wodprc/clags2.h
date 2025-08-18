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

/* Prototypes for external procedures generated from clags2.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void clags2_ (int_least32_t *upper, float *a1, __GFORTRAN_FLOAT_COMPLEX *a2, float *a3, float *b1, __GFORTRAN_FLOAT_COMPLEX *b2, float *b3, float *csu, __GFORTRAN_FLOAT_COMPLEX *snu, float *csv, __GFORTRAN_FLOAT_COMPLEX *snv, float *csq, __GFORTRAN_FLOAT_COMPLEX *snq);

#ifdef __cplusplus
}
#endif
