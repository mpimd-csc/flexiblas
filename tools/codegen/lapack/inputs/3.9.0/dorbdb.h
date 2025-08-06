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

/* Prototypes for external procedures generated from dorbdb.f

   Use of this interface is discouraged, consider using the
   BIND(C) feature of standard Fortran instead.  */

void dorbdb_ (char *trans, char *signs, int *m, int *p, int *q, double *x11, int *ldx11, double *x12, int *ldx12, double *x21, int *ldx21, double *x22, int *ldx22, double *theta, double *phi, double *taup1, double *taup2, double *tauq1, double *tauq2, double *work, int *lwork, int *info, size_t trans_len, size_t signs_len);

#ifdef __cplusplus
}
#endif
