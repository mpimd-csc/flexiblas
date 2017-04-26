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

#ifdef  INTEGER8 
	#include <stdint.h>
	#define Int int64_t 
#else 
	#define Int int 
#endif 
void F77_zaxpy(const Int *N, const void *alpha, void *X,
                    const Int *incX, void *Y, const Int *incY)
{
   cblas_zaxpy(*N, alpha, X, *incX, Y, *incY);
   return;
}

void F77_zcopy(const Int *N, void *X, const Int *incX, 
                    void *Y, const Int *incY)
{
   cblas_zcopy(*N, X, *incX, Y, *incY);
   return;
}

void F77_zdotc(const Int *N, const void *X, const Int *incX, 
                     const void *Y, const Int *incY,void *dotc)
{
   cblas_zdotc_sub(*N, X, *incX, Y, *incY, dotc);
   return;
}

void F77_zdotu(const Int *N, void *X, const Int *incX, 
                        void *Y, const Int *incY,void *dotu)
{
   cblas_zdotu_sub(*N, X, *incX, Y, *incY, dotu);
   return;
}

void F77_zdscal(const Int *N, const double *alpha, void *X,
                         const Int *incX)
{
   cblas_zdscal(*N, *alpha, X, *incX);
   return;
}

void F77_zscal(const Int *N, const void * *alpha, void *X,
                         const Int *incX)
{
   cblas_zscal(*N, alpha, X, *incX);
   return;
}

void F77_zswap( const Int *N, void *X, const Int *incX,
                          void *Y, const Int *incY)
{
   cblas_zswap(*N,X,*incX,Y,*incY);
   return;
}

Int F77_izamax(const Int *N, const void *X, const Int *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return(cblas_izamax(*N, X, *incX)+1);
}

double F77_dznrm2(const Int *N, const void *X, const Int *incX)
{
   return cblas_dznrm2(*N, X, *incX);
}

double F77_dzasum(const Int *N, void *X, const Int *incX)
{
   return cblas_dzasum(*N, X, *incX);
}
