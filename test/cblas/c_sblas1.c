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

#ifdef  INTEGER8 
	#include <stdint.h>
	#define Int int64_t 
#else 
	#define Int int 
#endif 
float F77_sasum(const Int *N, float *X, const Int *incX)
{
   return cblas_sasum(*N, X, *incX);
}

void F77_saxpy(const Int *N, const float *alpha, const float *X,
                    const Int *incX, float *Y, const Int *incY)
{
   cblas_saxpy(*N, *alpha, X, *incX, Y, *incY);
   return;
}

float F77_scasum(const Int *N, void *X, const Int *incX)
{
   return cblas_scasum(*N, X, *incX);
}

float F77_scnrm2(const Int *N, const void *X, const Int *incX)
{
   return cblas_scnrm2(*N, X, *incX);
}

void F77_scopy(const Int *N, const float *X, const Int *incX, 
                    float *Y, const Int *incY)
{
   cblas_scopy(*N, X, *incX, Y, *incY);
   return;
}

float F77_sdot(const Int *N, const float *X, const Int *incX, 
                        const float *Y, const Int *incY)
{
   return cblas_sdot(*N, X, *incX, Y, *incY);
}

float F77_snrm2(const Int *N, const float *X, const Int *incX)
{
   return cblas_snrm2(*N, X, *incX);
}

void F77_srotg( float *a, float *b, float *c, float *s)
{
   cblas_srotg(a,b,c,s);
   return;
}

void F77_srot( const Int *N, float *X, const Int *incX, float *Y,
              const Int *incY, const float  *c, const float  *s)
{
   cblas_srot(*N,X,*incX,Y,*incY,*c,*s);
   return;
}

void F77_sscal(const Int *N, const float *alpha, float *X,
                         const Int *incX)
{
   cblas_sscal(*N, *alpha, X, *incX);
   return;
}

void F77_sswap( const Int *N, float *X, const Int *incX,
                          float *Y, const Int *incY)
{
   cblas_sswap(*N,X,*incX,Y,*incY);
   return;
}

Int F77_isamax(const Int *N, const float *X, const Int *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return (cblas_isamax(*N, X, *incX)+1);
}
