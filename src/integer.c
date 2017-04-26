/* $Id: integer.c 3903 2013-11-25 16:10:53Z komart $ */ 
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
#include <complex.h> 

#include "hooks.h"
#ifndef __WIN32__
#include <dlfcn.h>
#ifndef RTLD_DEFAULT 
# define RTLD_DEFAULT   ((void *) 0)   
#endif 
#endif

struct flexiblas_blasfn flexiblas_icamax=HOOK_INIT;
struct flexiblas_blasfn flexiblas_idamax=HOOK_INIT;
struct flexiblas_blasfn flexiblas_isamax=HOOK_INIT;
struct flexiblas_blasfn flexiblas_izamax=HOOK_INIT;

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

static int user_xerbla = 0; 

static void internal_xerbla_(char *SNAME, Int *Info, Int len);  
#ifdef __ELF__
void xerbla_(char *, Int *, Int) __attribute__ ((weak, alias ("internal_xerbla_")));
void xerbla(char *, Int *, Int) __attribute__ ((weak, alias ("internal_xerbla_")));
#else 
void xerbla_(char *SNAME, Int *Info, Int len) {
	internal_xerbla_(SNAME, Info, len); 
}
void xerbla(char *SNAME, Int *Info, Int len) {
	internal_xerbla_(SNAME, Info, len); 
}
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
#ifndef __WIN32__
	/* Check if the user supplied a XERBLA function  */
	{
		// void *xerbla_symbol1 = dlsym(handle,"xerbla_"); 
		void *xerbla_symbol2 = dlsym(RTLD_DEFAULT,"xerbla_"); 
		void *internal = (void*) &internal_xerbla_; 
		/* printf("xerbla1_: %lx\n", (unsigned long)((void*)xerbla_symbol1));  
		printf("xerbla2_: %lx\n", (unsigned long)((void*)xerbla_symbol2));  
		printf("xerbla_: %lx\n", (unsigned long)((void*)&internal_xerbla_));   */

		if (internal == xerbla_symbol2) {
			user_xerbla = 0; 
		} else {
			user_xerbla = 1; 
		}


		if ( user_xerbla == 0 ){
			if (__flexiblas_verbose > 0 ) {
				fprintf(stderr, PRINT_PREFIX "Use XERBLA of the BLAS backend.\n"); 
			}
		} else {
			if (__flexiblas_verbose > 0 ) {
				fprintf(stderr, PRINT_PREFIX "Use XERBLA supplied by the user.\n"); 
			}
			flexiblas_xerbla.call_fblas = xerbla_symbol2; 
		}
	}
#endif 
	return 0; 
}


/*-----------------------------------------------------------------------------
 *  Define the function calls 
 *-----------------------------------------------------------------------------*/
BLAS_NONVOID_FN(Int,icamax,(Int *N, float complex *CX, Int *INCX),(N,CX,INCX));
BLAS_NONVOID_FN(Int,idamax,(Int *N, double *CX, Int *INCX),(N,CX,INCX));
BLAS_NONVOID_FN(Int,isamax,(Int *N, float *CX, Int *INCX),(N,CX,INCX));
BLAS_NONVOID_FN(Int,izamax,(Int *N, double complex *CX, Int *INCX),(N,CX,INCX));
// BLAS_FN(void, xerbla,(char * SNAME, Int *Info,Int len), (SNAME, Info, len)); 
static void internal_xerbla_(char *SNAME, Int *Info, Int len)  { 
	double ts;
	void (*fn) (char *SNAME, Int *info, Int len)  ;  
	fn = flexiblas_xerbla.call_fblas; 

	if ( fn == NULL ) { 
		int _info = (int) *Info; 
		char * ptr = malloc ( sizeof(char) * (len + 1)); 
		strncpy(ptr, SNAME, len); 
		ptr[len] = '\0'; 
		fprintf(stderr,"XERBLA: Parameter %d was incorrect on entry to %s\n", _info , ptr);
		free(ptr); 
		return; 
	}
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn (SNAME, Info, len);
		flexiblas_time_xerbla [0] += (flexiblas_wtime() -ts);
		flexiblas_call_xerbla [0]++;
	} else { 
		fn (SNAME, Info, len); 
	} 
	return; 
}



