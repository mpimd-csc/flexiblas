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

/* Prototypes for external procedures generated from zlarzb.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zlarzb_ (char *side, char *trans, char *direct, char *storev, int *m, int *n, int *k, int *l, __GFORTRAN_DOUBLE_COMPLEX *v, int *ldv, __GFORTRAN_DOUBLE_COMPLEX *t, int *ldt, __GFORTRAN_DOUBLE_COMPLEX *c, int *ldc, __GFORTRAN_DOUBLE_COMPLEX *work, int *ldwork, size_t side_len, size_t trans_len, size_t direct_len, size_t storev_len);

#ifdef __cplusplus
}
#endif
