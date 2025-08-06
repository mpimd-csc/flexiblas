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

/* Prototypes for external procedures generated from cstemr.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void cstemr_ (char *jobz, char *range, int *n, float *d, float *e, float *vl, float *vu, int *il, int *iu, int *m, float *w, __GFORTRAN_FLOAT_COMPLEX *z, int *ldz, int *nzc, int *isuppz, int_least32_t *tryrac, float *work, int *lwork, int *iwork, int *liwork, int *info, size_t jobz_len, size_t range_len);

#ifdef __cplusplus
}
#endif
