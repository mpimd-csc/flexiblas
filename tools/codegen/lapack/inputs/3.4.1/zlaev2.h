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

/* Prototypes for external procedures generated from zlaev2.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlaev2_ (__GFORTRAN_DOUBLE_COMPLEX *a, __GFORTRAN_DOUBLE_COMPLEX *b, __GFORTRAN_DOUBLE_COMPLEX *c, double *rt1, double *rt2, double *cs1, __GFORTRAN_DOUBLE_COMPLEX *sn1);

#ifdef __cplusplus
}
#endif
