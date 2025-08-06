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

/* Prototypes for external procedures generated from cunbdb1.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cunbdb1_ (int *m, int *p, int *q, __GFORTRAN_FLOAT_COMPLEX *x11, int *ldx11, __GFORTRAN_FLOAT_COMPLEX *x21, int *ldx21, float *theta, float *phi, __GFORTRAN_FLOAT_COMPLEX *taup1, __GFORTRAN_FLOAT_COMPLEX *taup2, __GFORTRAN_FLOAT_COMPLEX *tauq1, __GFORTRAN_FLOAT_COMPLEX *work, int *lwork, int *info);

#ifdef __cplusplus
}
#endif
