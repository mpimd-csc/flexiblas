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
void flexiblas_real_cladiv_( float complex* returnvalue, void* x, void* y)
#else
float complex flexiblas_real_cladiv_(void* x, void* y)
#endif 
{
	float complex (*fn) (void* x, void* y);
	void (*fn_intel) (float complex *ret, void* x, void* y);
	float complex ret;

	fn = current_backend->lapack.cladiv.fblas_real; 
	fn_intel = (void *) fn;

		if(current_backend->info.intel_interface == 0 ) {
			ret = fn((void*) x, (void*) y); 
		} else {
			fn_intel( &ret, (void*) x, (void*) y);
		}


#ifdef FLEXIBLAS_ABI_INTEL
    *((float complex *)returnvalue) = ret;
    return;
#else 
    return ret; 
#endif
}

#ifdef FLEXIBLAS_ABI_INTEL 
void flexiblas_real_cladiv( float complex* returnvalue, void* x, void* y)  __attribute__((alias("flexiblas_real_cladiv_")));

#else 
float complex flexiblas_real_cladiv(void* x, void* y)  __attribute__((alias("flexiblas_real_cladiv_")));

#endif



