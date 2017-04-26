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
void FC_GLOBAL(slags2,SLAGS2)(blasint* upper, float* a1, float* a2, float* a3, float* b1, float* b2, float* b3, float* csu, float* snu, float* csv, float* snv, float* csq, float* snq)
#else
void FC_GLOBAL(slags2,SLAGS2)(blasint* upper, float* a1, float* a2, float* a3, float* b1, float* b2, float* b3, float* csu, float* snu, float* csv, float* snv, float* csq, float* snq)
#endif 
{
    double ts;
	void (*fn) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.slags2.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq); 
		current_backend->lapack.slags2.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.slags2.calls[0]++;
	} else { 
		fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void slags2_(blasint* upper, float* a1, float* a2, float* a3, float* b1, float* b2, float* b3, float* csu, float* snu, float* csv, float* snv, float* csq, float* snq) __attribute__((alias(MTS(FC_GLOBAL(slags2,SLAGS2)))));
#else
void slags2(blasint* upper, float* a1, float* a2, float* a3, float* b1, float* b2, float* b3, float* csu, float* snu, float* csv, float* snv, float* csq, float* snq) __attribute__((alias(MTS(FC_GLOBAL(slags2,SLAGS2)))));
#endif



