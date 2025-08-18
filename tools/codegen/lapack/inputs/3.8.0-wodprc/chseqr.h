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

/* Prototypes for external procedures generated from chseqr.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void chseqr_ (char *job, char *compz, int *n, int *ilo, int *ihi, __GFORTRAN_FLOAT_COMPLEX *h, int *ldh, __GFORTRAN_FLOAT_COMPLEX *w, __GFORTRAN_FLOAT_COMPLEX *z, int *ldz, __GFORTRAN_FLOAT_COMPLEX *work, int *lwork, int *info, size_t job_len, size_t compz_len);

#ifdef __cplusplus
}
#endif
