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
 * Copyright (C) Martin Koehler, 2015-2017
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Tue Mar 28 16:07:36 2017 */ 
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "fortran_mangle.h"

#include "flexiblas.h"


#ifdef INTEGER8
#define blasint int64_t
#else 
#define blasint int 
#endif



#ifdef FLEXIBLAS_ABI_INTEL 
void FC_GLOBAL(slaqsp,SLAQSP)(char* uplo, blasint* n, float* ap, float* s, float* scond, float* amax, char* equed)
#else
void FC_GLOBAL(slaqsp,SLAQSP)(char* uplo, blasint* n, float* ap, float* s, float* scond, float* amax, char* equed)
#endif 
{
    double ts;
	void (*fn) (void* uplo, void* n, void* ap, void* s, void* scond, void* amax, void* equed);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.slaqsp.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) uplo, (void*) n, (void*) ap, (void*) s, (void*) scond, (void*) amax, (void*) equed); 
		current_backend->lapack.slaqsp.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.slaqsp.calls[0]++;
	} else { 
		fn((void*) uplo, (void*) n, (void*) ap, (void*) s, (void*) scond, (void*) amax, (void*) equed); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void slaqsp_(char* uplo, blasint* n, float* ap, float* s, float* scond, float* amax, char* equed) __attribute__((alias(MTS(FC_GLOBAL(slaqsp,SLAQSP)))));
#else
void slaqsp(char* uplo, blasint* n, float* ap, float* s, float* scond, float* amax, char* equed) __attribute__((alias(MTS(FC_GLOBAL(slaqsp,SLAQSP)))));
#endif


