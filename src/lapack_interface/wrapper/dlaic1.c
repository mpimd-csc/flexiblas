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
 /* Generated: Tue Mar 28 16:07:34 2017 */ 
        
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
void FC_GLOBAL(dlaic1,DLAIC1)(blasint* job, blasint* j, double* x, double* sest, double* w, double* gamma, double* sestpr, double* s, double* c)
#else
void FC_GLOBAL(dlaic1,DLAIC1)(blasint* job, blasint* j, double* x, double* sest, double* w, double* gamma, double* sestpr, double* s, double* c)
#endif 
{
    double ts;
	void (*fn) (void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.dlaic1.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c); 
		current_backend->lapack.dlaic1.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.dlaic1.calls[0]++;
	} else { 
		fn((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaic1_(blasint* job, blasint* j, double* x, double* sest, double* w, double* gamma, double* sestpr, double* s, double* c) __attribute__((alias(MTS(FC_GLOBAL(dlaic1,DLAIC1)))));
#else
void dlaic1(blasint* job, blasint* j, double* x, double* sest, double* w, double* gamma, double* sestpr, double* s, double* c) __attribute__((alias(MTS(FC_GLOBAL(dlaic1,DLAIC1)))));
#endif



