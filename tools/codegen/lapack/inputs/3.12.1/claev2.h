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

/* Prototypes for external procedures generated from claev2.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void claev2_ (__GFORTRAN_FLOAT_COMPLEX *a, __GFORTRAN_FLOAT_COMPLEX *b, __GFORTRAN_FLOAT_COMPLEX *c, float *rt1, float *rt2, float *cs1, __GFORTRAN_FLOAT_COMPLEX *sn1);

#ifdef __cplusplus
}
#endif
