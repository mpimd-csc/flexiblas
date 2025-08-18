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

/* Prototypes for external procedures generated from cunbdb5.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cunbdb5_ (int *m1, int *m2, int *n, __GFORTRAN_FLOAT_COMPLEX *x1, int *incx1, __GFORTRAN_FLOAT_COMPLEX *x2, int *incx2, __GFORTRAN_FLOAT_COMPLEX *q1, int *ldq1, __GFORTRAN_FLOAT_COMPLEX *q2, int *ldq2, __GFORTRAN_FLOAT_COMPLEX *work, int *lwork, int *info);

#ifdef __cplusplus
}
#endif
