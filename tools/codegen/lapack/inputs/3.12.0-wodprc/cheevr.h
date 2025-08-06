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

/* Prototypes for external procedures generated from cheevr.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cheevr_ (char *jobz, char *range, char *uplo, int *n, __GFORTRAN_FLOAT_COMPLEX *a, int *lda, float *vl, float *vu, int *il, int *iu, float *abstol, int *m, float *w, __GFORTRAN_FLOAT_COMPLEX *z, int *ldz, int *isuppz, __GFORTRAN_FLOAT_COMPLEX *work, int *lwork, float *rwork, int *lrwork, int *iwork, int *liwork, int *info, size_t jobz_len, size_t range_len, size_t uplo_len);

#ifdef __cplusplus
}
#endif
