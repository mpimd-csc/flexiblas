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
 /* Generated: Tue Mar 28 16:07:33 2017 */ 
        
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
void FC_GLOBAL(clar2v,CLAR2V)(blasint* n, float complex* x, float complex* y, float complex* z, blasint* incx, float* c, float complex* s, blasint* incc)
#else
void FC_GLOBAL(clar2v,CLAR2V)(blasint* n, float complex* x, float complex* y, float complex* z, blasint* incx, float* c, float complex* s, blasint* incc)
#endif 
{
    double ts;
	void (*fn) (void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.clar2v.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) n, (void*) x, (void*) y, (void*) z, (void*) incx, (void*) c, (void*) s, (void*) incc); 
		current_backend->lapack.clar2v.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.clar2v.calls[0]++;
	} else { 
		fn((void*) n, (void*) x, (void*) y, (void*) z, (void*) incx, (void*) c, (void*) s, (void*) incc); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void clar2v_(blasint* n, float complex* x, float complex* y, float complex* z, blasint* incx, float* c, float complex* s, blasint* incc) __attribute__((alias(MTS(FC_GLOBAL(clar2v,CLAR2V)))));
#else
void clar2v(blasint* n, float complex* x, float complex* y, float complex* z, blasint* incx, float* c, float complex* s, blasint* incc) __attribute__((alias(MTS(FC_GLOBAL(clar2v,CLAR2V)))));
#endif



