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

/* Prototypes for external procedures generated from zhb2st_kernels.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zhb2st_kernels_ (char *uplo, int_least32_t *wantz, int *ttype, int *st, int *ed, int *sweep, int *n, int *nb, int *ib, __GFORTRAN_DOUBLE_COMPLEX *a, int *lda, __GFORTRAN_DOUBLE_COMPLEX *v, __GFORTRAN_DOUBLE_COMPLEX *tau, int *ldvt, __GFORTRAN_DOUBLE_COMPLEX *work, size_t uplo_len);

#ifdef __cplusplus
}
#endif
