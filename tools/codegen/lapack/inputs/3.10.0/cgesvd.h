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

/* Prototypes for external procedures generated from cgesvd.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cgesvd_ (char *jobu, char *jobvt, int *m, int *n, __GFORTRAN_FLOAT_COMPLEX *a, int *lda, float *s, __GFORTRAN_FLOAT_COMPLEX *u, int *ldu, __GFORTRAN_FLOAT_COMPLEX *vt, int *ldvt, __GFORTRAN_FLOAT_COMPLEX *work, int *lwork, float *rwork, int *info, size_t jobu_len, size_t jobvt_len);

#ifdef __cplusplus
}
#endif
