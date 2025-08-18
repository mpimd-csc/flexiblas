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

/* Prototypes for external procedures generated from dhgeqz.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dhgeqz_ (char *job, char *compq, char *compz, int *n, int *ilo, int *ihi, double *h, int *ldh, double *t, int *ldt, double *alphar, double *alphai, double *beta, double *q, int *ldq, double *z, int *ldz, double *work, int *lwork, int *info, size_t job_len, size_t compq_len, size_t compz_len);

#ifdef __cplusplus
}
#endif
