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

/* Prototypes for external procedures generated from stpmlqt.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void stpmlqt_ (char *side, char *trans, int *m, int *n, int *k, int *l, int *mb, float *v, int *ldv, float *t, int *ldt, float *a, int *lda, float *b, int *ldb, float *work, int *info, size_t side_len, size_t trans_len);

#ifdef __cplusplus
}
#endif
