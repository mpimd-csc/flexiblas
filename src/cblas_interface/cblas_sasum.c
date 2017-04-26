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

float cblas_sasum( const int N, const float *X, const int incX) 
{
   float asum;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else 
   #define F77_N N
   #define F77_incX incX
#endif
#ifdef FLEXIBLAS_PROFILE
   flexiblas_call_sasum[POS_CBLAS] ++;
#endif 

   if ( flexiblas_sasum.call_cblas != NULL ) {
#ifdef FLEXIBLAS_PROFILE
	   double te, ts = flexiblas_wtime(); 
#endif
	   float (*fn)(const int , const float *, const int ) = flexiblas_sasum.call_cblas;
	   asum = fn(N,X,incX); 
#ifdef FLEXIBLAS_PROFILE
	   te = flexiblas_wtime(); 
	   flexiblas_time_sasum[POS_CBLAS] += (te - ts); 
	   // printf("%20e \t %20e\t %20e\n", te, ts , te-ts);
#endif
   } else {
	   asum = F77_sasum( &F77_N, X, &F77_incX);
   }

   return asum;
}

