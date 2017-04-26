/*
 * c_dblas1.c
 *
 * The program is a C wrapper for dcblat1.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas_test.h"
#include "cblas.h"

#ifdef  INTEGER8 
	#include <stdint.h>
	#define Int int64_t 
#else 
	#define Int int 
#endif 
double F77_dasum(const Int *N, double *X, const Int *incX)
{
   return cblas_dasum(*N, X, *incX);
}

void F77_daxpy(const Int *N, const double *alpha, const double *X,
                    const Int *incX, double *Y, const Int *incY)
{
   cblas_daxpy(*N, *alpha, X, *incX, Y, *incY);
   return;
}

void F77_dcopy(const Int *N, double *X, const Int *incX, 
                    double *Y, const Int *incY)
{
   cblas_dcopy(*N, X, *incX, Y, *incY);
   return;
}

double F77_ddot(const Int *N, const double *X, const Int *incX,
                const double *Y, const Int *incY)
{
   return cblas_ddot(*N, X, *incX, Y, *incY);
}

double F77_dnrm2(const Int *N, const double *X, const Int *incX)
{
   return cblas_dnrm2(*N, X, *incX);
}

void F77_drotg( double *a, double *b, double *c, double *s)
{
   cblas_drotg(a,b,c,s);
   return;
}

void F77_drot( const Int *N, double *X, const Int *incX, double *Y,
       const Int *incY, const double *c, const double *s)
{

   cblas_drot(*N,X,*incX,Y,*incY,*c,*s);
   return;
}

void F77_dscal(const Int *N, const double *alpha, double *X,
                         const Int *incX)
{
   cblas_dscal(*N, *alpha, X, *incX);
   return;
}

void F77_dswap( const Int *N, double *X, const Int *incX,
                          double *Y, const Int *incY)
{
   cblas_dswap(*N,X,*incX,Y,*incY);
   return;
}

double F77_dzasum(const Int *N, void *X, const Int *incX)
{
   return cblas_dzasum(*N, X, *incX);
}

double F77_dznrm2(const Int *N, const void *X, const Int *incX)
{
   return cblas_dznrm2(*N, X, *incX);
}

Int F77_idamax(const Int *N, const double *X, const Int *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return (cblas_idamax(*N, X, *incX)+1);
}
