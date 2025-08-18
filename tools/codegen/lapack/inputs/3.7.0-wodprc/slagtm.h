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

/* Prototypes for external procedures generated from slagtm.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void slagtm_ (char *trans, int *n, int *nrhs, float *alpha, float *dl, float *d, float *du, float *x, int *ldx, float *beta, float *b, int *ldb, size_t trans_len);

#ifdef __cplusplus
}
#endif
