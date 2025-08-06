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

/* Prototypes for external procedures generated from zlags2.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlags2_ (int_least32_t *upper, double *a1, __GFORTRAN_DOUBLE_COMPLEX *a2, double *a3, double *b1, __GFORTRAN_DOUBLE_COMPLEX *b2, double *b3, double *csu, __GFORTRAN_DOUBLE_COMPLEX *snu, double *csv, __GFORTRAN_DOUBLE_COMPLEX *snv, double *csq, __GFORTRAN_DOUBLE_COMPLEX *snq);

#ifdef __cplusplus
}
#endif
