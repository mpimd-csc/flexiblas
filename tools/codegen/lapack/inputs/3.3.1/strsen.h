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

/* Prototypes for external procedures generated from strsen.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void strsen_ (char *job, char *compq, int_least32_t *select, int *n, float *t, int *ldt, float *q, int *ldq, float *wr, float *wi, int *m, float *s, float *sep, float *work, int *lwork, int *iwork, int *liwork, int *info, size_t job_len, size_t compq_len);

#ifdef __cplusplus
}
#endif
