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

/* Prototypes for external procedures generated from sgegv.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sgegv_ (char *jobvl, char *jobvr, int *n, float *a, int *lda, float *b, int *ldb, float *alphar, float *alphai, float *beta, float *vl, int *ldvl, float *vr, int *ldvr, float *work, int *lwork, int *info, size_t jobvl_len, size_t jobvr_len);

#ifdef __cplusplus
}
#endif
