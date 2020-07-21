/*
 * c_zblas1.c
 *
 * The program is a C wrapper for zcblat1.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas_test.h"
#include "cblas.h"
void F77_zaxpy(const F77_INT *N, const void *alpha, void *X,
                    const F77_INT *incX, void *Y, const F77_INT *incY)
{
   cblas_zaxpy(*N, alpha, X, *incX, Y, *incY);
   return;
}

void F77_zcopy(const F77_INT *N, void *X, const F77_INT *incX,
                    void *Y, const F77_INT *incY)
{
   cblas_zcopy(*N, X, *incX, Y, *incY);
   return;
}

void F77_zdotc(const F77_INT *N, const void *X, const F77_INT *incX,
                     const void *Y, const F77_INT *incY,void *dotc)
{
   cblas_zdotc_sub(*N, X, *incX, Y, *incY, dotc);
   return;
}

void F77_zdotu(const F77_INT *N, void *X, const F77_INT *incX,
                        void *Y, const F77_INT *incY,void *dotu)
{
   cblas_zdotu_sub(*N, X, *incX, Y, *incY, dotu);
   return;
}

void F77_zdscal(const F77_INT *N, const double *alpha, void *X,
                         const F77_INT *incX)
{
   cblas_zdscal(*N, *alpha, X, *incX);
   return;
}

void F77_zscal(const F77_INT *N, const void * *alpha, void *X,
                         const F77_INT *incX)
{
   cblas_zscal(*N, alpha, X, *incX);
   return;
}

void F77_zswap( const F77_INT *N, void *X, const F77_INT *incX,
                          void *Y, const F77_INT *incY)
{
   cblas_zswap(*N,X,*incX,Y,*incY);
   return;
}

F77_INT F77_izamax(const F77_INT *N, const void *X, const F77_INT *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return(cblas_izamax(*N, X, *incX)+1);
}

double F77_dznrm2(const F77_INT *N, const void *X, const F77_INT *incX)
{
   return cblas_dznrm2(*N, X, *incX);
}

double F77_dzasum(const F77_INT *N, void *X, const F77_INT *incX)
{
   return cblas_dzasum(*N, X, *incX);
}
