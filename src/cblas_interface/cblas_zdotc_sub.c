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
#include <stdio.h>
void cblas_zdotc_sub( const int N, const void *X, const int incX,
                    const void *Y, const int incY,void *dotc)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else 
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   current_backend->blas.zdotc_sub.calls[POS_CBLAS] ++;

   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.zdotc_sub.call_cblas != NULL ) {
	   double te = 0, ts = 0;
	   if ( __flexiblas_profile ){
	   	ts = flexiblas_wtime(); 
	   }
	   void (*fn)
		  ( const int N, const void *X, const int incX,
                    const void *Y, const int incY,void *dotc)
		   = current_backend->blas.zdotc_sub.call_cblas;
	fn(N,X,incX,Y,incY,dotc);
	if ( __flexiblas_profile ){
	   te = flexiblas_wtime(); 
	   current_backend->blas.zdotc_sub.timings[POS_CBLAS] += (te - ts); 
	}
   } else {
	double complex d; 
#ifdef USE_INTERFACE_INTEL
	F77_zdotc( &d, &F77_N, X, &F77_incX, Y, &F77_incY);
#else
	d = F77_zdotc( &F77_N, X, &F77_incX, Y, &F77_incY);
#endif
	*((double complex *) dotc) = d; 
   }
}
