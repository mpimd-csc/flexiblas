/*
 * cblas_zaxpby.c
 *
 * The program is a C interface to zaxpby.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
#include "cblas_f77_ext.h"
void API_SUFFIX(cblas_zaxpby)( const CBLAS_INT N, const void *alpha, const void *X,
                       const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_zaxpby( &F77_N, alpha, X, &F77_incX, beta, Y, &F77_incY);
}
