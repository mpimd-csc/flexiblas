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

/* Prototypes for external procedures generated from ssbev_2stage.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void ssbev_2stage_ (char *jobz, char *uplo, int *n, int *kd, float *ab, int *ldab, float *w, float *z, int *ldz, float *work, int *lwork, int *info, size_t jobz_len, size_t uplo_len);

#ifdef __cplusplus
}
#endif
