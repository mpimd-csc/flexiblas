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
#include "../extblas.h"

void cblas_comatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, 
		     const int crows, const int ccols, const void* calpha, const void* a, const int clda, 
		     void *b, const int cldb)
{
#ifdef F77_INT
   F77_INT F77_ROWS=crows; 
   F77_INT F77_COLS=ccols; 
   F77_INT F77_LDA =clda; 
   F77_INT F77_LDB =cldb; 
#else 
   #define F77_ROWS crows 
   #define F77_COLS ccols 
   #define F77_LDA  clda 
   #define F77_LDB  cldb 
#endif
   if ( current_backend->post_init != 0 ) {
   	__flexiblas_backend_init(current_backend);
   	current_backend->post_init = 0;
   }
   if ( current_backend->extblas.comatcopy.call_cblas != NULL ) {
	   float te = 0, ts = 0;
	   if ( __flexiblas_profile ) {
		   ts = flexiblas_wtime(); 
	   }
	   void (*fn)(const CBLAS_ORDER, const CBLAS_TRANSPOSE, const int, const int, const void *, const void*, const int, void *, const int) = current_backend->extblas.comatcopy.call_cblas;
	   fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);  
	   if ( __flexiblas_profile ){
		   te = flexiblas_wtime(); 
		   current_backend->extblas.comatcopy.timings[POS_CBLAS] += (te - ts); 
	   }
   } else {
	char ORDER[2]=" ";
	char TRANS[2]=" "; 
	switch(CORDER){
		case CblasColMajor:
			ORDER[0]='C';
			break; 
		case CblasRowMajor:
			ORDER[0]='R'; 
			break; 
		default: 
			ORDER[0]='X'; 
	}
	switch(CTRANS){
		case CblasNoTrans:
			TRANS[0]='N'; 
			break; 
		case CblasConjNoTrans:
			TRANS[0]='R'; 
			break; 
		case CblasTrans:
			TRANS[0]='T'; 
			break; 
		case CblasConjTrans:
			TRANS[0]='C';
			break; 
		default: 
			TRANS[0]='X'; 
	}
   	FC_GLOBAL(comatcopy,COMATCOPY)( ORDER, TRANS, &F77_ROWS, &F77_COLS, calpha, a, &F77_LDA, b, &F77_LDB);
   }
   current_backend->extblas.comatcopy.calls[POS_CBLAS] ++;
} 

