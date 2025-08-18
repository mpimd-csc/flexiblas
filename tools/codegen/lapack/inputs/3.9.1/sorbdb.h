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

/* Prototypes for external procedures generated from sorbdb.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void sorbdb_ (char *trans, char *signs, int *m, int *p, int *q, float *x11, int *ldx11, float *x12, int *ldx12, float *x21, int *ldx21, float *x22, int *ldx22, float *theta, float *phi, float *taup1, float *taup2, float *tauq1, float *tauq2, float *work, int *lwork, int *info, size_t trans_len, size_t signs_len);

#ifdef __cplusplus
}
#endif
