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

/* Prototypes for external procedures generated from zhpevx.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void zhpevx_ (char *jobz, char *range, char *uplo, int *n, __GFORTRAN_DOUBLE_COMPLEX *ap, double *vl, double *vu, int *il, int *iu, double *abstol, int *m, double *w, __GFORTRAN_DOUBLE_COMPLEX *z, int *ldz, __GFORTRAN_DOUBLE_COMPLEX *work, double *rwork, int *iwork, int *ifail, int *info, size_t jobz_len, size_t range_len, size_t uplo_len);

#ifdef __cplusplus
}
#endif
