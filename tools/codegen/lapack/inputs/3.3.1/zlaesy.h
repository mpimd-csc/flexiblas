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

/* Prototypes for external procedures generated from zlaesy.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlaesy_ (__GFORTRAN_DOUBLE_COMPLEX *a, __GFORTRAN_DOUBLE_COMPLEX *b, __GFORTRAN_DOUBLE_COMPLEX *c, __GFORTRAN_DOUBLE_COMPLEX *rt1, __GFORTRAN_DOUBLE_COMPLEX *rt2, __GFORTRAN_DOUBLE_COMPLEX *evscal, __GFORTRAN_DOUBLE_COMPLEX *cs1, __GFORTRAN_DOUBLE_COMPLEX *sn1);

#ifdef __cplusplus
}
#endif
