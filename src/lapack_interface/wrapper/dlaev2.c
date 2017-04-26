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
void FC_GLOBAL(dlaev2,DLAEV2)(double* a, double* b, double* c, double* rt1, double* rt2, double* cs1, double* sn1)
#else
void FC_GLOBAL(dlaev2,DLAEV2)(double* a, double* b, double* c, double* rt1, double* rt2, double* cs1, double* sn1)
#endif 
{
    double ts;
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.dlaev2.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); 
		current_backend->lapack.dlaev2.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.dlaev2.calls[0]++;
	} else { 
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaev2_(double* a, double* b, double* c, double* rt1, double* rt2, double* cs1, double* sn1) __attribute__((alias(MTS(FC_GLOBAL(dlaev2,DLAEV2)))));
#else
void dlaev2(double* a, double* b, double* c, double* rt1, double* rt2, double* cs1, double* sn1) __attribute__((alias(MTS(FC_GLOBAL(dlaev2,DLAEV2)))));
#endif



