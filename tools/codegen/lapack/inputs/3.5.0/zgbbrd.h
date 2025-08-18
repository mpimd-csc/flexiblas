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

/* Prototypes for external procedures generated from zgbbrd.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zgbbrd_ (char *vect, int *m, int *n, int *ncc, int *kl, int *ku, __GFORTRAN_DOUBLE_COMPLEX *ab, int *ldab, double *d, double *e, __GFORTRAN_DOUBLE_COMPLEX *q, int *ldq, __GFORTRAN_DOUBLE_COMPLEX *pt, int *ldpt, __GFORTRAN_DOUBLE_COMPLEX *c, int *ldc, __GFORTRAN_DOUBLE_COMPLEX *work, double *rwork, int *info, size_t vect_len);

#ifdef __cplusplus
}
#endif
