/*
 * c_sblas1.c
 *
 * The program is a C wrapper for scblat1.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas_test.h"
#include "cblas.h"
float F77_sasum(const F77_INT *N, float *X, const F77_INT *incX)
{
   return cblas_sasum(*N, X, *incX);
}

void F77_saxpy(const F77_INT *N, const float *alpha, const float *X,
                    const F77_INT *incX, float *Y, const F77_INT *incY)
{
   cblas_saxpy(*N, *alpha, X, *incX, Y, *incY);
   return;
}

float F77_scasum(const F77_INT *N, void *X, const F77_INT *incX)
{
   return cblas_scasum(*N, X, *incX);
}

float F77_scnrm2(const F77_INT *N, const void *X, const F77_INT *incX)
{
   return cblas_scnrm2(*N, X, *incX);
}

void F77_scopy(const F77_INT *N, const float *X, const F77_INT *incX,
                    float *Y, const F77_INT *incY)
{
   cblas_scopy(*N, X, *incX, Y, *incY);
   return;
}

float F77_sdot(const F77_INT *N, const float *X, const F77_INT *incX,
                        const float *Y, const F77_INT *incY)
{
   return cblas_sdot(*N, X, *incX, Y, *incY);
}

float F77_snrm2(const F77_INT *N, const float *X, const F77_INT *incX)
{
   return cblas_snrm2(*N, X, *incX);
}

void F77_srotg( float *a, float *b, float *c, float *s)
{
   cblas_srotg(a,b,c,s);
   return;
}

void F77_srot( const F77_INT *N, float *X, const F77_INT *incX, float *Y,
              const F77_INT *incY, const float  *c, const float  *s)
{
   cblas_srot(*N,X,*incX,Y,*incY,*c,*s);
   return;
}

void F77_sscal(const F77_INT *N, const float *alpha, float *X,
                         const F77_INT *incX)
{
   cblas_sscal(*N, *alpha, X, *incX);
   return;
}

void F77_sswap( const F77_INT *N, float *X, const F77_INT *incX,
                          float *Y, const F77_INT *incY)
{
   cblas_sswap(*N,X,*incX,Y,*incY);
   return;
}

F77_INT F77_isamax(const F77_INT *N, const float *X, const F77_INT *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return (cblas_isamax(*N, X, *incX)+1);
}
