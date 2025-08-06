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

/* Prototypes for external procedures generated from cupgtr.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cupgtr_ (char *uplo, int *n, __GFORTRAN_FLOAT_COMPLEX *ap, __GFORTRAN_FLOAT_COMPLEX *tau, __GFORTRAN_FLOAT_COMPLEX *q, int *ldq, __GFORTRAN_FLOAT_COMPLEX *work, int *info, size_t uplo_len);

#ifdef __cplusplus
}
#endif
