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


void cblas_srotmg( float *d1, float *d2, float *b1, 
                        const float b2, float *p)
{
   flexiblas_call_srotmg[POS_CBLAS] ++;

   if ( flexiblas_srotmg.call_cblas != NULL ) {
	   float te = 0, ts = 0;
	   if ( __flexiblas_profile ) {
		   ts = flexiblas_wtime(); 
	   }
	   void (*fn)
		  ( float *d1, float *d2, float *b1, 
                        const float b2, float *p)
		   = flexiblas_srotmg.call_cblas;
 	   fn(d1,d2,b1,b2,p);
	   if ( __flexiblas_profile ){
	   	te = flexiblas_wtime(); 
                flexiblas_time_srotmg[POS_CBLAS] += (te - ts); 
	   }
   } else {
	F77_srotmg(d1,d2,b1,&b2,p);
   }
   return; 
}
