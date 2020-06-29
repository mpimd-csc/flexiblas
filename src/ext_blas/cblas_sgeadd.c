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

void cblas_sgeadd(const CBLAS_ORDER CORDER,
		     const int crows, const int ccols, const float calpha, float *a, const int clda,
             const float cbeta, float *b, const int cldb)
{
#ifdef F77_INT
   F77_INT F77_LDA =clda;
   F77_INT F77_LDB =cldb;
#else
   #define F77_LDA  clda
   #define F77_LDB  cldb
#endif
   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->blas.sgeadd.call_cblas != NULL ) {
	   void (*fn)(const CBLAS_ORDER, const int, const int, const float, float *, const int, const float, float *, const int)
            = current_backend->blas.sgeadd.call_cblas;
	   fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
   } else {
#ifdef F77_INT
       F77_INT t = 0;
       F77_INT rows = crows;
       F77_INT cols = ccols;
#else
       int t = 0;
       int rows = crows;
       int cols = ccols;
#endif

       if ( CORDER == CblasRowMajor ) {
           t = rows;
           rows = cols;
           cols = t;
       }
       FC_GLOBAL(sgeadd,SGEADD)( &rows, &cols, &calpha, a, &F77_LDA, &cbeta, b, &F77_LDB);
   }
   current_backend->blas.sgeadd.calls[POS_CBLAS] ++;
}

