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

/* Prototypes for external procedures generated from cladiv.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

__GFORTRAN_FLOAT_COMPLEX cladiv_ (__GFORTRAN_FLOAT_COMPLEX *x, __GFORTRAN_FLOAT_COMPLEX *y);

#ifdef __cplusplus
}
#endif
