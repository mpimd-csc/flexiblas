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


void cblas_drotg(  double *a, double *b, double *c, double *s)
{
   current_backend->blas.drotg.calls[POS_CBLAS] ++;

   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.drotg.call_cblas != NULL ) {
	   double te = 0, ts = 0;
	   if ( __flexiblas_profile ) {
		   ts = flexiblas_wtime(); 
	   }
	   void (*fn)(double *, double *, double *, double *)
		   = current_backend->blas.drotg.call_cblas;
	   fn(a,b,c,s); 
	   if ( __flexiblas_profile ){
		   te = flexiblas_wtime(); 
		   current_backend->blas.drotg.timings[POS_CBLAS] += (te - ts); 
	   }
   } else {
	FC_GLOBAL(drotg,DROTG)(a,b,c,s);    
   }
   return; 
}

