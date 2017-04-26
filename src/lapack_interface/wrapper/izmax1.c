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
 /* Generated: Tue Mar 28 16:07:35 2017 */ 
        
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
int FC_GLOBAL(izmax1,IZMAX1)(blasint* n, double complex* zx, blasint* incx)
#else
int FC_GLOBAL(izmax1,IZMAX1)(blasint* n, double complex* zx, blasint* incx)
#endif 
{
    double ts;
	blasint (*fn) (void* n, void* zx, void* incx);
	blasint ret;
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.izmax1.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		ret = fn((void*) n, (void*) zx, (void*) incx); 
		current_backend->lapack.izmax1.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.izmax1.calls[0]++;
	} else { 
		ret = fn((void*) n, (void*) zx, (void*) incx); 
	} 
	return ret; 
}
#ifdef FLEXIBLAS_ABI_IBM
int izmax1_(blasint* n, double complex* zx, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(izmax1,IZMAX1)))));
#else
int izmax1(blasint* n, double complex* zx, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(izmax1,IZMAX1)))));
#endif


