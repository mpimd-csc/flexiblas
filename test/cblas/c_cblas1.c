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

#ifdef  INTEGER8 
	#include <stdint.h>
	#define Int int64_t 
#else 
	#define Int int 
#endif 

void F77_caxpy(const Int *N, const void *alpha, void *X,
                    const Int *incX, void *Y, const Int *incY)
{
   cblas_caxpy(*N, alpha, X, *incX, Y, *incY);
   return;
}


void F77_ccopy(const Int *N, void *X, const Int *incX, 
                    void *Y, const Int *incY)
{
   cblas_ccopy(*N, X, *incX, Y, *incY);
   return;
}

void F77_cdotc(const Int *N, void *X, const Int *incX, 
                        void *Y, const Int *incY, void *dotc)
{
   cblas_cdotc_sub(*N, X, *incX, Y, *incY, dotc);
   return;
}

void F77_cdotu(const Int *N, void *X, const Int *incX, 
                        void *Y, const Int *incY,void *dotu)
{
   cblas_cdotu_sub(*N, X, *incX, Y, *incY, dotu);
   return;
}

void F77_cscal(const Int *N, const void * *alpha, void *X,
                         const Int *incX)
{
   cblas_cscal(*N, alpha, X, *incX);
   return;
}

void F77_csscal(const Int *N, const float *alpha, void *X,
                         const Int *incX)
{
   cblas_csscal(*N, *alpha, X, *incX);
   return;
}

void F77_cswap( const Int *N, void *X, const Int *incX,
                          void *Y, const Int *incY)
{
   cblas_cswap(*N,X,*incX,Y,*incY);
   return;
}

Int F77_icamax(const Int *N, const void *X, const Int *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return (cblas_icamax(*N, X, *incX)+1);
}

float F77_scnrm2(const Int *N, const void *X, const Int *incX)
{
   return cblas_scnrm2(*N, X, *incX);
}

float F77_scasum(const Int *N, void *X, const Int *incX)
{
   return cblas_scasum(*N, X, *incX);
}
