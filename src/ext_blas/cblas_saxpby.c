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
#include "../extblas.h"

void cblas_saxpby( const int N, const float alpha, const float *X,
                       const int incX, const float beta, float *Y, const int incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else 
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   if ( flexiblas_saxpby.call_cblas != NULL ) {
	   float te = 0, ts = 0;
	   if ( __flexiblas_profile ) {
		   ts = flexiblas_wtime(); 
	   }
	   void (*fn)(const int , const float , const float *, const int,const float ,  const float *Y, const int ) = flexiblas_saxpby.call_cblas;
	   fn(N,alpha,X,incX,beta, Y,incY); 
	   if ( __flexiblas_profile ){
		   te = flexiblas_wtime(); 
		   flexiblas_time_saxpby[POS_CBLAS] += (te - ts); 
	   }
   } else {
   	F77_saxpby( &F77_N, &alpha, X, &F77_incX, &beta, Y, &F77_incY);
   }
   flexiblas_call_saxpby[POS_CBLAS] ++;

} 
