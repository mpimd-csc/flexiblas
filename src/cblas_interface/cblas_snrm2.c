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
#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"

float cblas_snrm2( const int N, const float *X, const int incX) 
{
   float nrm2;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else 
   #define F77_N N
   #define F77_incX incX
#endif
   current_backend->blas.snrm2.calls[POS_CBLAS] ++;

   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.snrm2.call_cblas != NULL ) {
	   float te = 0, ts = 0;
	   if ( __flexiblas_profile ) {
		   ts = flexiblas_wtime(); 
	   }
	   float (*fn) ( const int N, const float *X, const int incX) 
                 = current_backend->blas.snrm2.call_cblas;
	   nrm2 = fn(N,X,incX);
	   if ( __flexiblas_profile ){
		   te = flexiblas_wtime(); 
		   current_backend->blas.snrm2.timings[POS_CBLAS] += (te - ts); 
	   }
   } else {
	nrm2 = F77_snrm2( &F77_N, X, &F77_incX);
   }
   return nrm2;
}
