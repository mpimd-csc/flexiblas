/*
 * c_cblas1.c
 *
 * The program is a C wrapper for ccblat1.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas_test.h"
#include "cblas.h"
void F77_caxpy(const F77_INT *N, const void *alpha, void *X,
                    const F77_INT *incX, void *Y, const F77_INT *incY)
{
   cblas_caxpy(*N, alpha, X, *incX, Y, *incY);
   return;
}

void F77_ccopy(const F77_INT *N, void *X, const F77_INT *incX,
                    void *Y, const F77_INT *incY)
{
   cblas_ccopy(*N, X, *incX, Y, *incY);
   return;
}

void F77_cdotc(const F77_INT *N, void *X, const F77_INT *incX,
                        void *Y, const F77_INT *incY, void *dotc)
{
   cblas_cdotc_sub(*N, X, *incX, Y, *incY, dotc);
   return;
}

void F77_cdotu(const F77_INT *N, void *X, const F77_INT *incX,
                        void *Y, const F77_INT *incY,void *dotu)
{
   cblas_cdotu_sub(*N, X, *incX, Y, *incY, dotu);
   return;
}

void F77_cscal(const F77_INT *N, const void * *alpha, void *X,
                         const F77_INT *incX)
{
   cblas_cscal(*N, alpha, X, *incX);
   return;
}

void F77_csscal(const F77_INT *N, const float *alpha, void *X,
                         const F77_INT *incX)
{
   cblas_csscal(*N, *alpha, X, *incX);
   return;
}

void F77_cswap( const F77_INT *N, void *X, const F77_INT *incX,
                          void *Y, const F77_INT *incY)
{
   cblas_cswap(*N,X,*incX,Y,*incY);
   return;
}

F77_INT F77_icamax(const F77_INT *N, const void *X, const F77_INT *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return (cblas_icamax(*N, X, *incX)+1);
}

float F77_scnrm2(const F77_INT *N, const void *X, const F77_INT *incX)
{
   return cblas_scnrm2(*N, X, *incX);
}

float F77_scasum(const F77_INT *N, void *X, const F77_INT *incX)
{
   return cblas_scasum(*N, X, *incX);
}
