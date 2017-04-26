/* $Id: hooks.h 3741 2013-10-01 12:54:54Z komart $ */
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
#include "cblas.h"
#include "cblas_f77.h"
#include "../hooks.h"
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
#ifdef FLEXIBLAS_PROFILE
   flexiblas_call_cdotu[POS_CBLAS] ++;
#endif 

   if ( flexiblas_cdotu.call_cblas != NULL ) {
#ifdef FLEXIBLAS_PROFILE
	   double te, ts = flexiblas_wtime(); 
#endif
	   void (*fn)
		  ( const int N, const void *X,
                     const int incX, const void *Y, const int incY,void *dotu)
		   = flexiblas_cdotu.call_cblas;
	   fn(N,X,incX,Y,incY,dotu);
#ifdef FLEXIBLAS_PROFILE
	   te = flexiblas_wtime(); 
	   flexiblas_time_cdotu[POS_CBLAS] += (te - ts); 
#endif
   } else {
	float complex d; 
	d = F77_cdotu( &F77_N, X, &F77_incX, Y, &F77_incY);
	*((float complex *)dotu) = d;
   }
}
