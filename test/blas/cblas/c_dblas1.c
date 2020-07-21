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
double F77_dasum(const F77_INT *N, double *X, const F77_INT *incX)
{
   return cblas_dasum(*N, X, *incX);
}

void F77_daxpy(const F77_INT *N, const double *alpha, const double *X,
                    const F77_INT *incX, double *Y, const F77_INT *incY)
{
   cblas_daxpy(*N, *alpha, X, *incX, Y, *incY);
   return;
}

void F77_dcopy(const F77_INT *N, double *X, const F77_INT *incX,
                    double *Y, const F77_INT *incY)
{
   cblas_dcopy(*N, X, *incX, Y, *incY);
   return;
}

double F77_ddot(const F77_INT *N, const double *X, const F77_INT *incX,
                const double *Y, const F77_INT *incY)
{
   return cblas_ddot(*N, X, *incX, Y, *incY);
}

double F77_dnrm2(const F77_INT *N, const double *X, const F77_INT *incX)
{
   return cblas_dnrm2(*N, X, *incX);
}

void F77_drotg( double *a, double *b, double *c, double *s)
{
   cblas_drotg(a,b,c,s);
   return;
}

void F77_drot( const F77_INT *N, double *X, const F77_INT *incX, double *Y,
       const F77_INT *incY, const double *c, const double *s)
{

   cblas_drot(*N,X,*incX,Y,*incY,*c,*s);
   return;
}

void F77_dscal(const F77_INT *N, const double *alpha, double *X,
                         const F77_INT *incX)
{
   cblas_dscal(*N, *alpha, X, *incX);
   return;
}

void F77_dswap( const F77_INT *N, double *X, const F77_INT *incX,
                          double *Y, const F77_INT *incY)
{
   cblas_dswap(*N,X,*incX,Y,*incY);
   return;
}

double F77_dzasum(const F77_INT *N, void *X, const F77_INT *incX)
{
   return cblas_dzasum(*N, X, *incX);
}

double F77_dznrm2(const F77_INT *N, const void *X, const F77_INT *incX)
{
   return cblas_dznrm2(*N, X, *incX);
}

F77_INT F77_idamax(const F77_INT *N, const double *X, const F77_INT *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return (cblas_idamax(*N, X, *incX)+1);
}
