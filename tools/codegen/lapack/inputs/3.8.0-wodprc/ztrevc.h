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

/* Prototypes for external procedures generated from ztrevc.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void ztrevc_ (char *side, char *howmny, int_least32_t *select, int *n, __GFORTRAN_DOUBLE_COMPLEX *t, int *ldt, __GFORTRAN_DOUBLE_COMPLEX *vl, int *ldvl, __GFORTRAN_DOUBLE_COMPLEX *vr, int *ldvr, int *mm, int *m, __GFORTRAN_DOUBLE_COMPLEX *work, double *rwork, int *info, size_t side_len, size_t howmny_len);

#ifdef __cplusplus
}
#endif
