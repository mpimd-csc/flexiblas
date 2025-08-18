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

/* Prototypes for external procedures generated from dlatdf.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dlatdf_ (int *ijob, int *n, double *z, int *ldz, double *rhs, double *rdsum, double *rdscal, int *ipiv, int *jpiv);

#ifdef __cplusplus
}
#endif
