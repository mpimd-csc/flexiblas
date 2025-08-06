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

/* Prototypes for external procedures generated from dbdsvdx.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dbdsvdx_ (char *uplo, char *jobz, char *range, int *n, double *d, double *e, double *vl, double *vu, int *il, int *iu, int *ns, double *s, double *z, int *ldz, double *work, int *iwork, int *info, size_t uplo_len, size_t jobz_len, size_t range_len);

#ifdef __cplusplus
}
#endif
