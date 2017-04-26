/* $Id: flexiblas.h 3741 2013-10-01 12:54:54Z komart $ */
/* 
 Copyright (C) 2013  Martin Köhler, koehlerm@mpi-magdeburg.mpg.de

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
#include "flexiblas_config.h"
#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include <complex.h>

void cblas_cdotu_sub( const int N, const void *X,
                     const int incX, const void *Y, const int incY,void *dotu)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else 
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   current_backend->blas.cdotu_sub.calls[POS_CBLAS] ++;

   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.cdotu_sub.call_cblas != NULL ) {
	   double te = 0, ts = 0;
	   if (__flexiblas_profile) {
		ts = flexiblas_wtime(); 
	   }
	   void (*fn)
		  ( const int N, const void *X,
                     const int incX, const void *Y, const int incY,void *dotu)
		   = current_backend->blas.cdotu_sub.call_cblas;
	   fn(N,X,incX,Y,incY,dotu);
	   if ( __flexiblas_profile ) {
		   te = flexiblas_wtime(); 
		   current_backend->blas.cdotu_sub.timings[POS_CBLAS] += (te - ts); 
	   }
   } else {
	float complex d; 
#ifdef  FLEXIBLAS_ABI_INTEL
	FC_GLOBAL(cdotu,CDOTU)( &d, &F77_N, X, &F77_incX, Y, &F77_incY);
#else 
	d = FC_GLOBAL(cdotu,CDOTU)( &F77_N, X, &F77_incX, Y, &F77_incY);
#endif
	*((float complex *)dotu) = d;
   }
}

