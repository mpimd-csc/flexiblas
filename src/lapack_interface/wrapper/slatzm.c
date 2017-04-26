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
void FC_GLOBAL(slatzm,SLATZM)(char* side, blasint* m, blasint* n, float* v, blasint* incv, float* tau, float* c1, float* c2, blasint* ldc, float* work)
#else
void FC_GLOBAL(slatzm,SLATZM)(char* side, blasint* m, blasint* n, float* v, blasint* incv, float* tau, float* c1, float* c2, blasint* ldc, float* work)
#endif 
{
    double ts;
	void (*fn) (void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.slatzm.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work); 
		current_backend->lapack.slatzm.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.slatzm.calls[0]++;
	} else { 
		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void slatzm_(char* side, blasint* m, blasint* n, float* v, blasint* incv, float* tau, float* c1, float* c2, blasint* ldc, float* work) __attribute__((alias(MTS(FC_GLOBAL(slatzm,SLATZM)))));
#else
void slatzm(char* side, blasint* m, blasint* n, float* v, blasint* incv, float* tau, float* c1, float* c2, blasint* ldc, float* work) __attribute__((alias(MTS(FC_GLOBAL(slatzm,SLATZM)))));
#endif



