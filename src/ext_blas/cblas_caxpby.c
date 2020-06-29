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

void cblas_caxpby( const int N, const void *alpha, const void *X,
                       const int incX, const void *beta, void *Y, const int incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   current_backend->blas.caxpby.calls[POS_CBLAS] ++;

   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.caxpby.call_cblas != NULL ) {
	   void (*fn)
		 ( const int N, const void *alpha, const void *X,
                       const int incX, const void *beta, void *Y, const int incY)
		   = current_backend->blas.caxpby.call_cblas;
	   fn(N,alpha,X,incX,beta, Y,incY);
   } else {
	FC_GLOBAL(caxpby,CAXPBY)( &F77_N, alpha, X, &F77_incX, beta, Y, &F77_incY);
   }
}
