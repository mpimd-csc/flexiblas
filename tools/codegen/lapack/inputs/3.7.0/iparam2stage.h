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

/* Prototypes for external procedures generated from iparam2stage.F

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

int iparam2stage_ (int *ispec, char *name, char *opts, int *ni, int *nbi, int *ibi, int *nxi, size_t name_len, size_t opts_len);

#ifdef __cplusplus
}
#endif
