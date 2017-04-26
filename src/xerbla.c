/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2015
 */

#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <complex.h> 

#include "flexiblas.h"
#ifndef __WIN32__
#include <dlfcn.h>
#ifndef RTLD_DEFAULT 
# define RTLD_DEFAULT   ((void *) 0)   
#endif 
#endif

// static int user_xerbla = 0; 
void flexiblas_internal_xerbla(char *SNAME, Int *Info, Int len);  
#ifdef FLEXIBLAS_ABI_IBM 
#ifdef __ELF__
#pragma weak xerbla_
#pragma weak xerbla
void xerbla_(char *, Int *, Int) __attribute__ (( alias ("flexiblas_internal_xerbla")));
void xerbla(char *, Int *, Int) __attribute__ (( alias ("flexiblas_internal_xerbla")));
#else 
void xerbla_(char *SNAME, Int *Info, Int len) {
	flexiblas_internal_xerbla(SNAME, Info, len); 
}
void xerbla(char *SNAME, Int *Info, Int len) {
	flexiblas_internal_xerbla(SNAME, Info, len); 
}
#endif

#else 
#ifdef __ELF__
void xerbla_(char *, Int *, Int) __attribute__ ((weak, alias ("flexiblas_internal_xerbla")));
void xerbla(char *, Int *, Int) __attribute__ ((weak, alias ("flexiblas_internal_xerbla")));
#else 
#pragma weak xerbla_  
#pragma weak xerbla
void xerbla_(char *SNAME, Int *Info, Int len) {
	flexiblas_internal_xerbla(SNAME, Info, len); 
}
void xerbla(char *SNAME, Int *Info, Int len) {
	flexiblas_internal_xerbla(SNAME, Info, len); 
}
#endif
#endif 


int __flexiblas_setup_xerbla(flexiblas_backend_t *backend)
{
#ifndef __WIN32__
	/* Check if the user supplied a XERBLA function  */
	{
        int user_xerbla = 0;
		void *xerbla_symbol1 = dlsym(backend->library_handle,"xerbla_"); 
		void *xerbla_symbol2 = dlsym(RTLD_DEFAULT,"xerbla_"); 
		void *internal = (void*) &flexiblas_internal_xerbla; 
		DPRINTF(1, "Available XERBLA ( backend: 0x%lx, user defined: 0x%lx, FlexiBLAS: 0x%lx )\n", 
				(unsigned long)((void*)xerbla_symbol1), 
				(unsigned long)((void*)xerbla_symbol2), 
				(unsigned long)((void*)&flexiblas_internal_xerbla));  

		if (internal == xerbla_symbol2) {
			user_xerbla = 0; 
		} else {
			user_xerbla = 1; 
		}

		if ( user_xerbla == 0 ){
			if (__flexiblas_verbose > 0 ) {
				fprintf(stderr, PRINT_PREFIX "Use XERBLA of the BLAS backend.\n"); 
			}
			backend->xerbla.call_fblas = xerbla_symbol1; 
		} else {
			if (__flexiblas_verbose > 0 ) {
				fprintf(stderr, PRINT_PREFIX "Use XERBLA supplied by the user.\n"); 
			}
			backend->xerbla.call_fblas = xerbla_symbol2; 
		}
	}
#endif 
	return 0; 
}


void flexiblas_internal_xerbla(char *SNAME, Int *Info, Int len)  { 
	double ts;
	void (*fn) (char *SNAME, Int *info, Int len)  ;  
	fn = current_backend->xerbla.call_fblas; 

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
		current_backend->xerbla.timings[0] += (flexiblas_wtime() -ts);
		current_backend->xerbla.calls [0]++;
	} else { 
        /* printf("XERBLA fn =%lx\n", (size_t) fn); */
		fn (SNAME, Info, len); 
	} 
	return; 
}



