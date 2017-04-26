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
 /* Generated: Tue Mar 28 16:07:38 2017 */ 
        
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
void FC_GLOBAL(zladiv,ZLADIV)( double complex* returnvalue, double complex* x, double complex* y)
#else
double complex FC_GLOBAL(zladiv,ZLADIV)(double complex* x, double complex* y)
#endif 
{
    double ts;
	double complex (*fn) (void* x, void* y);
	void (*fn_intel) (double complex *ret, void* x, void* y);
	double complex ret;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.zladiv.call_fblas; 
	fn_intel = (void *) fn;
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		if(current_backend->info.intel_interface == 0 ) {
			ret = fn((void*) x, (void*) y); 
		} else {
			fn_intel( &ret, (void*) x, (void*) y);
		}
		current_backend->lapack.zladiv.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.zladiv.calls[0]++;
	} else { 
		if(current_backend->info.intel_interface == 0 ) {
			ret = fn((void*) x, (void*) y); 
		} else {
			fn_intel( &ret, (void*) x, (void*) y);
		}
	} 

#ifdef FLEXIBLAS_ABI_INTEL 
    *returnvalue = ret; 
    return; 
#else 
    return ret;
#endif 
}
#ifdef FLEXIBLAS_ABI_IBM
double complex zladiv_(double complex* x, double complex* y) __attribute__((alias(MTS(FC_GLOBAL(zladiv,ZLADIV)))));
#else
double complex zladiv(double complex* x, double complex* y) __attribute__((alias(MTS(FC_GLOBAL(zladiv,ZLADIV)))));
#endif



