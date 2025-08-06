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

/* Prototypes for external procedures generated from cla_gbrcond_c.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

float cla_gbrcond_c_ (char *trans, int *n, int *kl, int *ku, __GFORTRAN_FLOAT_COMPLEX *ab, int *ldab, __GFORTRAN_FLOAT_COMPLEX *afb, int *ldafb, int *ipiv, float *c, int_least32_t *capply, int *info, __GFORTRAN_FLOAT_COMPLEX *work, float *rwork, size_t trans_len);

#ifdef __cplusplus
}
#endif
