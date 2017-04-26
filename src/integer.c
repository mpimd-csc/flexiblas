/* $Id$ */ 
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


#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>
#include <complex.h> 

#include "hooks.h"

struct flexiblas_blasfn flexiblas_icamax=HOOK_INIT;
struct flexiblas_blasfn flexiblas_idamax=HOOK_INIT;
struct flexiblas_blasfn flexiblas_isamax=HOOK_INIT;
struct flexiblas_blasfn flexiblas_izamax=HOOK_INIT;

#ifdef FLEXIBLAS_PROFILE
double  flexiblas_time_icamax[2] = {0.0,0.0};
double  flexiblas_time_idamax[2] = {0.0,0.0};
double  flexiblas_time_isamax[2] = {0.0,0.0};
double  flexiblas_time_izamax[2] = {0.0,0.0};
double  flexiblas_time_xerbla[2] = {0.0,0.0};

unsigned long  flexiblas_call_icamax[2] = {0,0};
unsigned long  flexiblas_call_idamax[2] = {0,0};
unsigned long  flexiblas_call_isamax[2] = {0,0};
unsigned long  flexiblas_call_izamax[2] = {0,0};
unsigned long  flexiblas_call_xerbla[2] = {0,0};

#endif
/*-----------------------------------------------------------------------------
 *  Load the Hooks for every function 
 *-----------------------------------------------------------------------------*/
int __flexiblas_hook_integer(void * handle){
	LOAD_HOOK(icamax);
	LOAD_HOOK(idamax);
	LOAD_HOOK(isamax);
	LOAD_HOOK(izamax);
 	LOAD_HOOK(xerbla); 	
	return 0; 
}


/*-----------------------------------------------------------------------------
 *  Define the function calls 
 *-----------------------------------------------------------------------------*/
BLAS_NONVOID_FN(Int,icamax,(Int *N, float complex *CX, Int *INCX),(N,CX,INCX));
BLAS_NONVOID_FN(Int,idamax,(Int *N, double *CX, Int *INCX),(N,CX,INCX));
BLAS_NONVOID_FN(Int,isamax,(Int *N, float *CX, Int *INCX),(N,CX,INCX));
BLAS_NONVOID_FN(Int,izamax,(Int *N, double complex *CX, Int *INCX),(N,CX,INCX));
BLAS_FN(void, xerbla,(char * SNAME, Int *Info,Int len), (SNAME, Info, len)); 


